type bin_op = Mul | Div | Add | Sub | Mod | Cons;;

type ast_raw =
    | Float of float
    | Bool of bool
    | Symbol of string
    | BinOp of bin_op * ast * ast
    | Call of ast * ast list
    | Let of bool * string * string list * ast * ast option
    | If of ast * ast * ast
and ast = { filename: string; line: int; col: int; ast: ast_raw };;

let print_ast =
    let rec helper indentation a =
        for _ = 1 to indentation do
            print_string "    ";
        done;
        Printf.printf "%s:%i:%i " a.filename a.line a.col;
        match a.ast with
        | Float f          ->
            print_float f;
            print_newline ();
        | Bool b           ->
            print_string (if b then "true\n" else "false\n");
        | Symbol s         ->
            print_string s;
            print_newline ();
        | BinOp (op, a, b) ->
            begin
                match op with
                | Mul  -> print_string "*"
                | Div  -> print_string "/"
                | Add  -> print_string "+"
                | Sub  -> print_string "-"
                | Mod  -> print_string "%"
                | Cons -> print_string "::"
            end;
            print_newline ();
            helper (indentation + 1) a;
            helper (indentation + 1) b;
        | Call (f, args)   ->
            print_string "call\n";
            helper (indentation + 1) f;
            List.iter (helper (indentation + 1)) args;
        | Let (recursive, name, args, value, context) ->
            Printf.printf "let%s %s" (if recursive then " rec" else "") name;
            List.iter (Printf.printf " %s") args;
            print_newline ();
            helper (indentation + 1) value;
            begin
                match context with
                | Some context -> helper (indentation + 1) context;
                | None         -> ()
            end
        | If (cond, theny, elsy) ->
            print_string "if\n";
            helper (indentation + 1) cond;
            helper (indentation + 1) theny;
            helper (indentation + 1) elsy;
    in helper 0;;

let parse_linfix ops next l =
    let rec helper a l =
        let state = Lexer.push_lexer l in
            match Lexer.lex l with
            | Ok { filename; line; col; token } ->
                if List.mem token ops then
                    match next l with
                    | Ok v ->
                        begin
                            let op = match token with
                                     | Lexer.Star        -> Mul
                                     | Lexer.Slash       -> Div
                                     | Lexer.Percent     -> Mod
                                     | Lexer.Plus        -> Add
                                     | Lexer.Minus       -> Sub
                                     | Lexer.DoubleColon -> Cons
                                     | _ -> raise Exit
                            in helper { filename; line; col; ast = BinOp (op, a, v) } l
                        end
                    | Error e ->
                        Lexer.pop_lexer l state;
                        Error e
                else begin
                    Lexer.pop_lexer l state;
                    Ok a
                end
            | Error e ->
                Lexer.pop_lexer l state;
                Error e
    in match next l with
    | Ok v -> helper v l
    | Error e -> Error e;;

let parse_rinfix ops next l =
    let rec helper a o l =
        let state = Lexer.push_lexer l in
            match Lexer.lex l with
            | Ok { filename; line; col; token } ->
                if List.mem token ops then
                    match next l with
                    | Ok v ->
                        begin
                            let op = match token with
                                     | Lexer.Star        -> Mul
                                     | Lexer.Slash       -> Div
                                     | Lexer.Percent     -> Mod
                                     | Lexer.Plus        -> Add
                                     | Lexer.Minus       -> Sub
                                     | Lexer.DoubleColon -> Cons
                                     | _ -> raise Exit
                            in helper (v :: a) ((op, filename, line, col) :: o) l
                        end
                    | Error e ->
                        Lexer.pop_lexer l state;
                        Error e
                else begin
                    Lexer.pop_lexer l state;
                    match a with
                    | x :: xs ->
                        let rec helper acc a o =
                            match (a, o) with
                            | ([], [])                                  -> acc
                            | (x :: xs, (op, filename, line, col) :: o) -> helper { filename; line; col; ast = BinOp (op, x, acc) } xs o
                            | _                                         -> raise Exit
                        in Ok (helper x xs o)
                    | _       -> raise Exit
                end
            | Error e ->
                Lexer.pop_lexer l state;
                Error e
    in match next l with
    | Ok v -> helper [v] [] l
    | Error e -> Error e;;

let rec parse_let l =
    let state = Lexer.push_lexer l in
        match Lexer.lex l with
        | Ok { filename; line; col; token = Lexer.Let } ->
            let recursive = match Lexer.peek l with
                            | Ok { token = Lexer.Rec; _ } ->
                                let _ = Lexer.lex l in
                                true
                            | _ -> false
            in begin
                match Lexer.lex l with
                | Ok { token = Lexer.Symbol var; _ } ->
                    let rec helper l args =
                        match Lexer.peek l with
                        | Ok { token = Lexer.Symbol s; _ } ->
                            let _ = Lexer.lex l in
                                helper l (s :: args)
                        | _ -> List.rev args
                    in let args = helper l [] in begin
                        match Lexer.lex l with
                        | Ok { token = EqualSign; _ } ->
                            begin
                                match parse_top l with
                                | Ok value ->
                                    begin
                                        match Lexer.peek l with
                                        | Ok { token = Lexer.In; _ } ->
                                            let _ = Lexer.lex l in
                                            begin
                                                match parse_top l with
                                                | Ok context -> Ok { filename; line; col; ast = Let (recursive, var, args, value, Some context) }
                                                | Error e ->
                                                    Lexer.pop_lexer l state;
                                                    Error e
                                            end
                                        | _ -> Ok { filename; line; col; ast = Let (recursive, var, args, value, None) }
                                    end
                                | Error e -> Error e
                            end
                        | _ ->
                            Lexer.pop_lexer l state;
                            Error "invalid let"
                    end
                | _ ->
                    Lexer.pop_lexer l state;
                    Error "invalid let"
            end
        | _ ->
            Lexer.pop_lexer l state;
            Error "invalid let"

and parse_if l =
    let state = Lexer.push_lexer l in
        match Lexer.lex l with
        | Ok { filename; line; col; token = Lexer.If } ->
            begin
                match parse_top l with
                | Ok cond ->
                    begin
                        match Lexer.lex l with
                        | Ok { token = Lexer.Then; _ } ->
                            begin
                                match parse_top l with
                                | Ok theny ->
                                    begin
                                        match Lexer.lex l with
                                        | Ok { token = Lexer.Else; _ } ->
                                            begin
                                                match parse_top l with
                                                | Ok elsy ->
                                                        Ok { filename; line; col; ast = If (cond, theny, elsy) }
                                                | Error e ->
                                                    Lexer.pop_lexer l state;
                                                    Error e
                                            end
                                        | _ ->
                                            Lexer.pop_lexer l state;
                                            Error "invalid if"
                                    end
                                | Error e ->
                                    Lexer.pop_lexer l state;
                                    Error e
                            end
                        | _ ->
                            Lexer.pop_lexer l state;
                            Error "invalid if"
                    end
                | Error e ->
                    Lexer.pop_lexer l state;
                    Error e
            end
        | _ ->
            Lexer.pop_lexer l state;
            Error "invalid if"

and parse_value l =
    match Lexer.lex l with
    | Ok { filename; line; col; token = Lexer.Float f }  -> Ok { filename; line; col; ast = Float f }
    | Ok { filename; line; col; token = Lexer.Bool b }  -> Ok { filename; line; col; ast = Bool b }
    | Ok { filename; line; col; token = Lexer.Symbol s } -> Ok { filename; line; col; ast = Symbol s }
    | Ok { token = Lexer.LParen; _ }                     ->
        begin
            match parse_add l with
            | Ok v    ->
                begin
                    match Lexer.lex l with
                    | Ok { token = Lexer.RParen; _ } -> Ok v
                    | Ok _                           -> Error "oh no"
                    | Error e                        -> Error e
                end
            | Error e -> Error e
        end
    | Ok _                                              -> Error "oh no"
    | Error e                                           -> Error e

and parse_call l =
    let state = Lexer.push_lexer l in
        match parse_value l with
        | Ok v    ->
            let rec helper a l =
                let state = Lexer.push_lexer l in
                match parse_value l with
                | Ok v    -> helper (v :: a) l
                | Error _ ->
                    Lexer.pop_lexer l state;
                    List.rev a
            in begin
                match helper [] l with
                | [] -> Ok v
                | l  -> Ok ({ filename = v.filename; line = v.line; col = v.col; ast = Call (v, l) })
            end
        | Error e ->
            Lexer.pop_lexer l state;
            Error e

and parse_mult l = parse_linfix [Lexer.Star; Lexer.Slash; Lexer.Percent] parse_call l
and parse_add l = parse_linfix [Lexer.Plus; Lexer.Minus] parse_mult l
and parse_cons l = parse_rinfix [Lexer.DoubleColon] parse_add l
and parse_top l =
    match parse_let l with
    | Ok v -> Ok v
    | Error _ -> match parse_if l with
    | Ok v -> Ok v
    | Error _ -> parse_cons l;;

let parse filename contents =
    let l = Lexer.create_lexer filename contents
    in parse_top l;;
