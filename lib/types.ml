(*
let rec find_functions asts funcs type_var_counter =
    match asts with
    | ast :: asts ->
        begin
            match ast with
            | Parse.Let (_, var, _, _, _) ->
                let funcs = ((var, Parse.TypeVar !type_var_counter) :: funcs) in
                let () = type_var_counter := !type_var_counter + 1 in
                find_functions asts funcs type_var_counter
            | _ -> find_functions asts funcs type_var_counter
        end
    | [] -> funcs;;
*)

let new_type_var type_var_counter =
    let t = Parse.TypeVar !type_var_counter in
    let () = type_var_counter := !type_var_counter + 1 in
        t

let rec assign_typevars (ast: Parse.ast) scopes substitutions type_var_counter =
    ast.ty <- new_type_var type_var_counter;
    match ast.ast with
    | Parse.Float _ -> substitutions := (ast.ty, Parse.TypeName ("float", [])) :: !substitutions
    | Parse.Bool _ -> substitutions := (ast.ty, Parse.TypeName ("bool", [])) :: !substitutions
    | Parse.Symbol s ->
        begin
            match List.find_map
            (function
             | ("_", _) -> None
             | (k, v) -> if k = s then Some v else None)
            !scopes with
            | Some t -> substitutions := (ast.ty, t) :: !substitutions
            | None -> Printf.printf "unknown symbol %s\n" s;
        end

    | Parse.BinOp (op, a, b) ->
        assign_typevars a scopes substitutions type_var_counter;
        assign_typevars b scopes substitutions type_var_counter;
        begin
            match op with
            | Parse.Cons ->
                let t = new_type_var type_var_counter in
                substitutions := (a.ty, t) :: (b.ty, Parse.TypeName ("list", [t])) :: (b.ty, ast.ty) :: !substitutions
            | _ -> substitutions := (ast.ty, Parse.TypeName ("float", [])) :: (a.ty, Parse.TypeName ("float", [])) :: (b.ty, Parse.TypeName ("float", [])) :: !substitutions
        end

    | Parse.Call (f, args) ->
        assign_typevars f scopes substitutions type_var_counter;
        List.iter (fun v -> assign_typevars v scopes substitutions type_var_counter) args;
        let rec helper (args: Parse.ast list) =
            match args with
            | x :: xs -> Parse.Function (x.ty, (helper xs))
            | [] -> ast.ty
        in substitutions := (f.ty, helper args) :: !substitutions;

    | Parse.Let (recursive, var, args, value, context) ->
        let ret_type = new_type_var type_var_counter in 
        let rec helper args =
            match args with
            | [] -> ret_type
            | _ :: xs -> Parse.Function (new_type_var type_var_counter, helper xs)
        in let t = helper args in
        begin
            let arg_types = List.map (fun arg -> (arg, new_type_var type_var_counter)) args in
            let new_scope = ref (arg_types @ !scopes) in
            begin
                if recursive then
                    new_scope := (var, t) :: !new_scope
                else ();
                assign_typevars value new_scope substitutions type_var_counter;
                substitutions := (List.fold_right
                    (function
                     | (_, x) -> fun y -> Parse.Function (x, y)) arg_types value.ty, t) :: !substitutions;
            end;
            match context with
            | Some context -> assign_typevars context (ref ((var, t) :: !scopes)) substitutions type_var_counter
            | None -> scopes := (var, t) :: !scopes;
        end;
        substitutions := (value.ty, ret_type) :: (ast.ty, Parse.TypeName ("unit", [])) :: !substitutions;

    | Parse.If (cond, theny, elsy) ->
        assign_typevars cond scopes substitutions type_var_counter;
        assign_typevars theny scopes substitutions type_var_counter;
        assign_typevars elsy scopes substitutions type_var_counter;
        substitutions := (cond.ty, Parse.TypeName ("bool", [])) :: (theny.ty, elsy.ty) :: !substitutions;

    | Parse.Match (_value, _pats) -> ()
    | Parse.TypeSumDef (_name, _variants) -> ()
    | Parse.TypeDef (_name, _ty) -> ()
    | Parse.Many values ->
        List.iter (fun x -> assign_typevars x scopes substitutions type_var_counter) values;;

let typecheck asts =
    let type_var_counter = ref 0 in
    let substitutions = ref [] in
    let scopes = ref [] in begin
        List.iter (fun ast -> assign_typevars ast scopes substitutions type_var_counter) asts;
        List.iter Parse.print_ast asts;
        print_string "substitutions = [";
        List.iter (function
                   | (t1, t2) -> print_string "("; Parse.print_type t1; print_string " = "; Parse.print_type t2; print_string "), ") !substitutions;
        print_string "]\n";
        print_string "scopes = [";
        List.iter (function
                   | (var, t2) -> Printf.printf "(%s = " var; Parse.print_type t2; print_string "), ") !scopes;
        print_string "]\n";
    end
