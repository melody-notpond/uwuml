type bin_op = Mul | Div | Add | Sub | Mod | Cons;;

type ast_raw =
    | Float of float
    | Symbol of string
    | BinOp of bin_op * ast * ast
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

let rec parse_value l =
    match Lexer.lex l with
    | Ok { filename; line; col; token = Lexer.Float f }  -> Ok { filename; line; col; ast = Float f }
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

and parse_mult l = parse_linfix [Lexer.Star; Lexer.Slash; Lexer.Percent] parse_value l
and parse_add l = parse_linfix [Lexer.Plus; Lexer.Minus] parse_mult l
and parse_cons l = parse_rinfix [Lexer.DoubleColon] parse_add l;;

let parse filename contents =
    let l = Lexer.create_lexer filename contents
    in parse_cons l;;
