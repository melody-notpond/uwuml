let new_type_var substitutions =
    let t = Parse.TypeVar (List.length !substitutions) in
    let () = substitutions := !substitutions @ [ref Parse.Unknown] in
        t;;

let rec set_substitution_constraint substitutions t1 t2 =
    match (t1, t2) with
    | (Parse.TypeVar n, t) | (t, Parse.TypeVar n) ->
        let rec helper n t =
            let sub = List.nth !substitutions n in
                match !sub with
                | Parse.Unknown ->
                    sub := t;
                | Parse.TypeVar n -> helper n t;
                | u -> set_substitution_constraint substitutions u t
        in helper n t
    | (Parse.TypeName (n1, g1), Parse.TypeName (n2, g2)) when n1 = n2 ->
        List.iter2 (set_substitution_constraint substitutions) g1 g2;
    | (Parse.Product a, Parse.Product b) ->
        List.iter2 (set_substitution_constraint substitutions) a b;
    | (Parse.Function (f, a), Parse.Function (g, b)) ->
        set_substitution_constraint substitutions f g;
        set_substitution_constraint substitutions a b;
    | (t1, t2) ->
        (* TODO: return an error type *)
        print_string "type mismatch: ";
        Parse.print_type t1;
        print_string " vs ";
        Parse.print_type t2;
        print_newline ();
        raise Exit;;

let rec assign_typevars (ast: Parse.ast) scopes substitutions =
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
        assign_typevars a scopes substitutions;
        assign_typevars b scopes substitutions;
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
        assign_typevars f scopes substitutions;
        List.iter (fun v -> assign_typevars v scopes substitutions) args;
        let rec helper (args: Parse.ast list) =
            match args with
            | x :: xs -> Parse.Function (x.ty, (helper xs))
            | [] -> ast.ty
        in set_substitution_constraint substitutions f.ty (helper args);

    | Parse.Let (recursive, var, args, value, context) ->
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
                assign_typevars value new_scope substitutions;
                set_substitution_constraint substitutions (List.fold_right
                    (function
                     | (_, x) -> fun y -> Parse.Function (x, y)) arg_types value.ty) t;
            end;
            match context with
            | Some context -> assign_typevars context (ref ((var, t) :: !scopes)) substitutions 
            | None -> scopes := (var, t) :: !scopes;
        end;
        set_substitution_constraint substitutions value.ty ret_type;
        set_substitution_constraint substitutions ast.ty (Parse.TypeName ("unit", []));

    | Parse.If (cond, theny, elsy) ->
        assign_typevars cond scopes substitutions;
        assign_typevars theny scopes substitutions;
        assign_typevars elsy scopes substitutions;
        set_substitution_constraint substitutions cond.ty (Parse.TypeName ("bool", []));
        set_substitution_constraint substitutions theny.ty elsy.ty;

    | Parse.Match (_value, _pats) -> ()
    | Parse.TypeSumDef (_name, _variants) -> ()
    | Parse.TypeDef (_name, _ty) -> ()
    | Parse.Many values ->
        List.iter (fun x -> assign_typevars x scopes substitutions) values;;

let rec flatten_substitutions_helper n subs subs_total =
    match subs with
    | [] -> false
    | x :: xs ->
        let v =
            match !x with
            | Parse.TypeVar m when n <> m ->
                x := !(List.nth subs_total m);
                true
            | t ->
                let modified = ref false in
                let rec helper t =
                    match t with
                    | Parse.TypeVar m when n <> m ->
                        modified := true;
                        !(List.nth subs_total m)
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
    | _ ->  ();;

let typecheck asts =
    let substitutions = ref [] in
    let scopes = ref [] in begin
        List.iter (fun ast -> assign_typevars ast scopes substitutions) asts;
        flatten_substitutions !substitutions;
        List.iter (fun x -> set_ast_types x !substitutions) asts;
        List.iter Parse.print_ast asts;
        (*
        print_string "substitutions = [";
        List.iteri (fun i t -> print_string "($"; print_int i; print_string " = "; Parse.print_type !t; print_string "), ") !substitutions;
        print_string "]\n";
        *)
        Ok ()
    end;;
