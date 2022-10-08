(* type bin_op = Mul | Div | Add | Sub | Mod | Cons;; *)

type ast_raw =
    | Float of float
    (* | BinOp of bin_op * ast * ast *)
and ast = { filename: string; line: int; col: int; ast: ast_raw };;

let print_ast =
    let helper indentation a =
        for _ = 1 to indentation do
            print_string "    ";
        done;
        Printf.printf "%s:%i:%i " a.filename a.line a.col;
        match a.ast with
        | Float f -> print_float f
        (*
        | BinOp (op, a, b) ->
            match op with
            | Mul -> print_string "*"
            | Div -> print_string "/"
            | Add -> print_string "+"
            | Sub -> print_string "-"
            | Mod -> print_string "%"
            | Cons -> print_string "::";
            helper (indentation + 1) a;
            helper (indentation + 1) b;
            *);
        print_newline ()
    in helper 0;;

let parse filename contents =
    let l = Lexer.create_lexer filename contents in
        match Lexer.lex l with
        | Ok { filename; line; col; token = Lexer.Float f } -> Ok { filename; line; col; ast = Float f }
        | Ok _                                              -> Error "oh no"
        | Error e                                           -> Error e;;
