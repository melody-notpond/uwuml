type bin_op = Mul | Div | Add | Sub | Mod | Cons;;

type ast_raw =
    | Float of float
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

let parse_value l =
    match Lexer.lex l with
    | Ok { filename; line; col; token = Lexer.Float f } -> Ok { filename; line; col; ast = Float f }
    | Ok _                                              -> Error "oh no"
    | Error e                                           -> Error e;;

let parse_mult l =
    let rec helper a l =
        let state = Lexer.push_lexer l in
            match Lexer.lex l with
            | Ok { filename; line; col; token = Lexer.Star | Lexer.Slash | Lexer.Percent as token } ->
                begin
                    match parse_value l with
                    | Ok v ->
                        begin
                            let op = match token with
                                     | Lexer.Star    -> Mul
                                     | Lexer.Slash   -> Div
                                     | Lexer.Percent -> Mod
                                     | _ -> raise Exit
                            in helper { filename; line; col; ast = BinOp (op, a, v) } l
                        end
                    | Error e ->
                        Lexer.pop_lexer l state;
                        Error e
                end
            | Ok _    ->
                Lexer.pop_lexer l state;
                Ok a
            | Error e ->
                Lexer.pop_lexer l state;
                Error e
    in match parse_value l with
    | Ok v -> helper v l
    | Error e -> Error e;;

let parse filename contents =
    let l = Lexer.create_lexer filename contents
    in parse_mult l
