type func_graph_node = { mutable depends_on: string list; mutable dependents: string list };;
type func_graph = { nodes: (string * func_graph_node) list; top_levels: string list };;

let generate_func_graph (asts: Parse.ast list) =
    let rec helper (asts: Parse.ast list) =
        match asts with
        | { ast = Parse.Let (_, var, _, _, None); _ } :: xs ->
            (var, { depends_on = []; dependents = [] }) :: helper xs
        | _ :: xs -> helper xs
        | []      -> []
    in let nodes = helper asts in
    let rec helper name blacklist nodes (ast: Parse.ast) =
        match ast.ast with
        | Parse.Symbol s when s <> name && not (List.mem s blacklist) ->
            begin
                match List.assoc_opt s nodes with
                | Some dependency when name <> "" ->
                    let current = List.assoc name nodes in
                        current.depends_on <- s :: current.depends_on;
                        dependency.dependents <- name :: dependency.dependents;
                | _ -> ()
            end
        | Parse.BinOp (_, a, b) ->
            helper name blacklist nodes a;
            helper name blacklist nodes b;
        | Parse.Call (f, args) ->
            helper name blacklist nodes f;
            List.iter (helper name blacklist nodes) args;
        | Parse.Let (_recursive, var, args, value, context) ->
            begin
                match context with
                | Some context ->
                    helper name (var :: args @ blacklist) nodes value;
                    helper name (var :: args @ blacklist) nodes context;
                | None         ->
                    helper var (args @ blacklist) nodes value;
            end
        | Parse.If (cond, theny, elsy) ->
            helper name blacklist nodes cond;
            helper name blacklist nodes theny;
            helper name blacklist nodes elsy;
        | Parse.Match (_value, _pats) -> ()
        | Parse.Many values ->
            List.iter (helper name blacklist nodes) values;
        | _ -> ()

    in List.iter (helper "" [] nodes) asts;
    (* TODO: mutually recursive functions at top level *)
    { nodes; top_levels = List.filter_map (function
                                           | (v, { dependents = []; _ }) -> Some v
                                           | _ -> None) nodes };;

let new_type_var substitutions =
    let t = Parse.TypeVar (List.length !substitutions) in
    let () = substitutions := !substitutions @ [ref Parse.Unknown] in
        t;;

let set_substitution_constraint substitutions t1 t2 =
    let generic_mappings = ref [] in
    let rec helper t =
        match t with
        | Parse.TypeVar n ->
            let sub = List.nth !substitutions n in
                if !sub = t then
                    match List.assoc_opt n !generic_mappings with
                    | Some t -> t
                    | None ->
                        let changed_to = new_type_var substitutions in
                        generic_mappings := (n, changed_to) :: !generic_mappings;
                        changed_to
                else if !sub = Parse.Unknown then
                    t
                else helper !sub
        | Parse.TypeName (n, g) ->
            Parse.TypeName (n, List.map helper g);
        | Parse.Product a ->
            Parse.Product (List.map helper a);
        | Parse.Function (f, a) ->
            Parse.Function (helper f, helper a)
        | _ -> t
    in let t1 = helper t1 in
    let t2 = helper t2 in
    let rec helper t1 t2 =
        match (t1, t2) with
        | (Parse.TypeVar n, t) | (t, Parse.TypeVar n) ->
            let rec helper2 n t =
                let sub = List.nth !substitutions n in
                    match !sub with
                    | Parse.Unknown ->
                        sub := t;
                    | Parse.TypeVar m ->
                        if n <> m then
                            helper2 m t
                        else sub := t;
                    | u ->
                        helper u t
            in helper2 n t
        | (Parse.TypeName (n1, g1), Parse.TypeName (n2, g2)) when n1 = n2 ->
            List.iter2 helper g1 g2;
        | (Parse.Product a, Parse.Product b) ->
            List.iter2 helper a b;
        | (Parse.Function (f, a), Parse.Function (g, b)) ->
            helper f g;
            helper a b;
        | (t1, t2) ->
            (* TODO: return an error type *)
            print_string "type mismatch: ";
            Parse.print_type t1;
            print_string " vs ";
            Parse.print_type t2;
            print_newline ();
        raise Exit
    in helper t1 t2;;

let rec assign_typevars (ast: Parse.ast) scopes substitutions handle_top =
    if ast.ty = Parse.Unknown then begin
        ast.ty <- new_type_var substitutions;
        match ast.ast with
        | Parse.Float _ -> set_substitution_constraint substitutions ast.ty (Parse.TypeName ("float", []))
        | Parse.Bool _ -> set_substitution_constraint substitutions ast.ty (Parse.TypeName ("bool", []))
        | Parse.Symbol s ->
            begin
                match List.find_map
                (function
                 | ("_", _) -> None
                 | (k, v) -> if k = s then Some v else None)
                !scopes with
                | Some t -> set_substitution_constraint substitutions ast.ty t
                | None -> Printf.printf "unknown symbol %s\n" s;
            end

        | Parse.BinOp (op, a, b) ->
            assign_typevars a scopes substitutions handle_top;
            assign_typevars b scopes substitutions handle_top;
            begin
                match op with
                | Parse.Cons ->
                    let t = new_type_var substitutions in begin
                        set_substitution_constraint substitutions a.ty t;
                        set_substitution_constraint substitutions b.ty (Parse.TypeName ("list", [t]));
                        set_substitution_constraint substitutions ast.ty (Parse.TypeName ("list", [t]));
                    end
                | Parse.Add | Parse.Sub | Parse.Mul | Parse.Div | Parse.Mod ->
                    set_substitution_constraint substitutions a.ty (Parse.TypeName ("float", []));
                    set_substitution_constraint substitutions b.ty (Parse.TypeName ("float", []));
                    set_substitution_constraint substitutions ast.ty (Parse.TypeName ("float", []));
                | Parse.Gt | Parse.Lt | Parse.Ge | Parse.Le | Parse.Eq | Parse.Ne ->
                    set_substitution_constraint substitutions a.ty (Parse.TypeName ("float", []));
                    set_substitution_constraint substitutions b.ty (Parse.TypeName ("float", []));
                    set_substitution_constraint substitutions ast.ty (Parse.TypeName ("bool", []));
            end

        | Parse.Call (f, args) ->
            assign_typevars f scopes substitutions handle_top;
            List.iter (fun v -> assign_typevars v scopes substitutions handle_top) args;
            let rec helper (args: Parse.ast list) =
                match args with
                | x :: xs -> Parse.Function (x.ty, (helper xs))
                | [] -> ast.ty
            in set_substitution_constraint substitutions f.ty (helper args);

        | Parse.Let (recursive, var, args, value, Some context) ->
            let ret_type = new_type_var substitutions in 
            let rec helper args =
                match args with
                | [] -> ret_type
                | _ :: xs -> Parse.Function (new_type_var substitutions, helper xs)
            in let t = helper args in
            begin
                let arg_types = List.map (fun arg -> (arg, new_type_var substitutions)) args in
                let new_scope = ref (arg_types @ !scopes) in
                begin
                    if recursive then
                        new_scope := (var, t) :: !new_scope
                    else ();
                    assign_typevars value new_scope substitutions handle_top;
                    set_substitution_constraint substitutions (List.fold_right
                        (function
                         | (_, x) -> fun y -> Parse.Function (x, y)) arg_types value.ty) t;
                end;
                assign_typevars context (ref ((var, t) :: !scopes)) substitutions handle_top
            end;
            set_substitution_constraint substitutions value.ty ret_type;
            set_substitution_constraint substitutions ast.ty (Parse.TypeName ("unit", []));

        | Parse.Let (recursive, var, args, value, None) when handle_top ->
            let ret_type = new_type_var substitutions in 
            let rec helper args =
                match args with
                | [] -> ret_type
                | _ :: xs -> Parse.Function (new_type_var substitutions, helper xs)
            in let t = helper args in
            begin
                let arg_types = List.map (fun arg -> (arg, new_type_var substitutions)) args in
                let new_scope = ref (arg_types @ !scopes) in
                begin
                    if recursive then
                        new_scope := (var, t) :: !new_scope
                    else ();
                    assign_typevars value new_scope substitutions handle_top;
                    set_substitution_constraint substitutions (List.fold_right
                        (function
                         | (_, x) -> fun y -> Parse.Function (x, y)) arg_types value.ty) t;
                end;
                scopes := (var, t) :: !scopes;
            end;
            set_substitution_constraint substitutions value.ty ret_type;
            set_substitution_constraint substitutions ast.ty (Parse.TypeName ("unit", []));

        | Parse.Let (_, _, _, _, _) -> ()

        | Parse.If (cond, theny, elsy) ->
            assign_typevars cond scopes substitutions handle_top;
            assign_typevars theny scopes substitutions handle_top;
            assign_typevars elsy scopes substitutions handle_top;
            set_substitution_constraint substitutions cond.ty (Parse.TypeName ("bool", []));
            set_substitution_constraint substitutions theny.ty elsy.ty;

        | Parse.Match (_value, _pats) -> ()
        | Parse.TypeSumDef (_name, _variants) ->
            set_substitution_constraint substitutions ast.ty (Parse.TypeName ("unit", []));
        | Parse.TypeDef (_name, _ty) ->
            set_substitution_constraint substitutions ast.ty (Parse.TypeName ("unit", []));
        | Parse.Many values ->
            List.iter (fun x -> assign_typevars x scopes substitutions handle_top) values
    end;;

let rec flatten_substitutions_helper n subs subs_total =
    match subs with
    | [] -> false
    | x :: xs ->
        let v =
            match !x with
            | Parse.TypeVar m when n <> m ->
                x := !(List.nth subs_total m);
                Parse.TypeVar m <> !x;
            | Parse.Unknown -> false
            | t ->
                let modified = ref false in
                let rec helper t =
                    match t with
                    | Parse.TypeVar m when n <> m ->
                        let t = !(List.nth subs_total m) in
                        let () = modified := !modified || t <> Parse.TypeVar m in
                        t
                    | Parse.TypeName (name, ts) ->
                        Parse.TypeName (name, List.map helper ts)
                    | Parse.Product ts ->
                        Parse.Product (List.map helper ts)
                    | Parse.Function (f, a) ->
                        Parse.Function (helper f, helper a)
                    | t -> t
                in x := helper t;
                !modified
        in flatten_substitutions_helper (n + 1) xs subs_total || v;;

let rec flatten_substitutions subs =
    if flatten_substitutions_helper 0 subs subs then
        flatten_substitutions subs
    else ();;

let rec set_ast_types (ast: Parse.ast) substitutions =
    begin
        match ast.ty with
        | Parse.TypeVar n -> ast.ty <- !(List.nth substitutions n)
        | _ -> ()
    end;
    match ast.ast with
    | Parse.BinOp (_, a, b) ->
        set_ast_types a substitutions;
        set_ast_types b substitutions;

    | Parse.Call (f, args) ->
        set_ast_types f substitutions;
        List.iter (fun x -> set_ast_types x substitutions) args;

    | Parse.Let (_, _, _, value, context) ->
        set_ast_types value substitutions;
        begin
            match context with
            | Some context -> set_ast_types context substitutions;
            | None -> ();
        end

    | Parse.If (cond, theny, elsy) ->
        set_ast_types cond substitutions;
        set_ast_types theny substitutions;
        set_ast_types elsy substitutions;

    | Parse.Match (_value, _pats) -> ()
    | Parse.TypeSumDef (_name, _variants) -> ()
    | Parse.TypeDef (_name, _ty) -> ()
    | Parse.Many values ->
        List.iter (fun x -> set_ast_types x substitutions) values
    | _ -> ();;

let rec find_constructors (asts: Parse.ast list) =
    match asts with
    | [] -> []
    | x :: xs ->
        begin
            match x.ast with
            | Parse.TypeSumDef (name, variants) ->
                List.map (function
                          | (n, Some (Parse.Product ts)) -> (n, List.fold_right (fun x y -> Parse.Function (x, y)) ts (Parse.TypeName (name, [])))
                          | (n, Some t) -> (n, Parse.Function (t, Parse.TypeName (name, [])))

                          | (n, None) -> (n, Parse.TypeName (name, []))) variants @ find_constructors xs;
            | _ ->
                find_constructors xs;
        end;;

let rec walk_graph asts substitutions scopes graph name =
    let node = List.assoc name graph.nodes in
    let () = List.iter (walk_graph asts substitutions scopes graph) node.depends_on in
    let rec helper (asts: Parse.ast list) =
        match asts with
        | { ast = Parse.Let (_, var, _, _, _); _ } as ast :: _ when var = name ->
            assign_typevars ast scopes substitutions true;
            flatten_substitutions !substitutions;
            set_ast_types ast !substitutions;
        | _ :: xs -> helper xs
        | [] -> raise Exit
    in helper asts;;

let typecheck asts =
    let substitutions = ref [] in
    let graph = generate_func_graph asts in
    let scopes = ref (find_constructors asts) in begin
        List.iter (walk_graph asts substitutions scopes graph) graph.top_levels;
        List.iter (fun ast -> assign_typevars ast scopes substitutions false) asts;
        flatten_substitutions !substitutions;
        List.iter (fun x -> set_ast_types x !substitutions) asts;
        List.iter Parse.print_ast asts;
        print_string "substitutions = [";
        List.iteri (fun i t -> print_string "($"; print_int i; print_string " = "; Parse.print_type !t; print_string "), ") !substitutions;
        print_string "]\n";
        Ok ()
    end;;
