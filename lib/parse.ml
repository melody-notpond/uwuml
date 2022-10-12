type bin_op = Mul | Div | Add | Sub | Mod | Cons;;
type pattern_raw =
    | Wildcard
    | SymbolPat of string
    | FloatPat of float
    | BoolPat of bool
    | ConsPat of pattern * pattern
    | SumPat of string * pattern list
    | ManyPat of pattern * pattern
and pattern = { filename: string; line: int; col: int; pattern: pattern_raw };;
type ast_raw =
    | Float of float
    | Bool of bool
    | Symbol of string
    | BinOp of bin_op * ast * ast
    | Call of ast * ast list
    | Let of bool * string * string list * ast * ast option
    | If of ast * ast * ast
    | Match of ast * (pattern * ast) list
and ast = { filename: string; line: int; col: int; ast: ast_raw };;

let rec print_pattern p =
    match p.pattern with
    | Wildcard -> print_string "_"
    | SymbolPat s -> print_string s
    | ConsPat (x, xs) ->
        print_string "(";
        print_pattern x;
        print_string " :: ";
        print_pattern xs;
        print_string ")";
    | SumPat (name, pats) ->
        print_string "(";
        print_string name;
        List.iter (fun p -> print_string " "; print_pattern p) pats;
        print_string ")"
    | ManyPat (a, b) ->
        print_string "(";
        print_pattern a;
        print_string " | ";
        print_pattern b;
        print_string "("
    | FloatPat f ->
        print_float f
    | BoolPat b ->
        print_string (if b then "true" else "false");;

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
        | Match (value, pats) ->
            print_string "match\n";
            helper (indentation + 1) value;
            List.iter (function
                       | (p, v) -> print_pattern p; print_newline (); helper (indentation + 1) v) pats;
    in helper 0;;

let (let*) o f =
    match o with
    | Ok v    -> f v
    | Error e -> Error e;;

let (<*) f g l =
    let state = Lexer.push_lexer l in
        match f l with
        | Ok v ->
            begin
                match g l with
                | Ok _    -> Ok v
                | Error e ->
                    Lexer.pop_lexer l state;
                    Error e
            end
        | Error e ->
            Lexer.pop_lexer l state;
            Error e;;

let ( *> ) f g l =
    let state = Lexer.push_lexer l in
        match f l with
        | Ok _ ->
            begin
                match g l with
                | Ok v    -> Ok v
                | Error e ->
                    Lexer.pop_lexer l state;
                    Error e
            end
        | Error e ->
            Lexer.pop_lexer l state;
            Error e;;

let (|*) f g l =
    match f l with
    | Ok v -> Ok v
    | Error _ -> g l;;

let (&*) f g l =
    let state = Lexer.push_lexer l in
        match f l with
        | Ok v ->
            begin
                match g l with
                | Ok u -> Ok (v, u)
                | Error e ->
                    Lexer.pop_lexer l state;
                    Error e
            end
        | Error e ->
            Lexer.pop_lexer l state;
            Error e;;

let consume_token t l =
    let state = Lexer.push_lexer l in
        match (Lexer.lex l, t) with
        | (Ok ({ token; _ } as tok), _) when token == t -> Ok tok
        | (Ok ({ token = Lexer.Float _; _ } as tok), Lexer.Float _) -> Ok tok
        | (Ok ({ token = Lexer.Bool _; _ } as tok), Lexer.Bool _) -> Ok tok
        | (Ok ({ token = Lexer.Symbol _; _ } as tok), Lexer.Symbol _) -> Ok tok
        | _ ->
            Lexer.pop_lexer l state;
            Error "mismatched token";;

let optional f l =
    Ok (Result.to_option (f l));;

let many_zero f l =
    let rec helper a f l =
        match f l with
        | Ok v    -> helper (v :: a) f l
        | Error _ -> List.rev a
    in Ok (helper [] f l);;

let many_one f l =
    let rec helper a f l =
        match f l with
        | Ok v    -> helper (v :: a) f l
        | Error _ -> List.rev a
    in match helper [] f l with
       | [] -> Error "cannot be empty"
       | v  -> Ok v;;

let map f g l =
    match f l with
    | Ok v    -> Ok (g v)
    | Error e -> Error e

(*
let rec parse_pattern_value l =
    let state = Lexer.push_lexer l in
        match Lexer.lex l with
        | Ok { filename; line; col; token = Lexer.Symbol "_" } -> Ok { filename; line; col; pattern = Wildcard }
        | Ok { filename; line; col; token = Lexer.Symbol s } -> Ok { filename; line; col; pattern = SymbolPat s }
        | Ok { filename; line; col; token = Lexer.Float f } -> Ok { filename; line; col; pattern = FloatPat f }
        | Ok { filename; line; col; token = Lexer.Bool b } -> Ok { filename; line; col; pattern = BoolPat b }
        | Ok { token = Lexer.LParen; _ } ->
            begin
                match parse_pattern l with
                | Ok v -> begin
                    match Lexer.lex l with
                    | Ok { token = Lexer.RParen; _ } -> Ok v
                    | _ ->
                        Lexer.pop_lexer l state;
                        Error "invalid pattern"
                end
                | Error e -> Error e
            end
        | _ ->
            Lexer.pop_lexer l state;
            Error "invalid pattern";

and parse_pattern_sum l =
    let state = Lexer.push_lexer l in
        match Lexer.lex l with
        | Ok { filename; line; col; token = Lexer.Symbol v } ->
            let rec helper a l =
                let state = Lexer.push_lexer l in
                match parse_pattern_value l with
                | Ok v    -> helper (v :: a) l
                | Error _ ->
                    Lexer.pop_lexer l state;
                    List.rev a
            in begin
                match helper [] l with
                | [] ->
                    Lexer.pop_lexer l state;
                    Error "invalid sum pattern"
                | l  -> Ok { filename; line; col; pattern = SumPat (v, l) }
            end
        | _ ->
            Lexer.pop_lexer l state;
            Error "invalid sum pattern"

and parse_pattern l =
    Error "";;
*)

let map_helper (v: Lexer.token) =
    match v.token with
    | Lexer.Symbol s -> s
    | _ -> raise Exit;;

let rec parse_let l =
    map (((((consume_token Lexer.Let &* optional (consume_token Lexer.Rec) &* consume_token (Lexer.Symbol "") &* many_zero (consume_token (Lexer.Symbol ""))) <* consume_token Lexer.EqualSign) &* parse_top) <* consume_token Lexer.In) &* optional parse_top)
        (function
         | ((((({ filename; line; col; _ }, recursive), { token = Lexer.Symbol var; _ }), args), value), context) ->
            { filename; line; col; ast = Let (Option.is_some recursive, var, List.map map_helper args, value, context) }
         | _ -> raise Exit)
        l

and parse_if l =
    map (consume_token Lexer.If &* parse_top &* (consume_token Lexer.Then *> parse_top) &* (consume_token Lexer.Else *> parse_top))
        (function
            | ((({ filename; line; col; _ }, cond), theny), elsy) -> { filename; line; col; ast = If (cond, theny, elsy) })
        l

(*
and parse_match l =
    let state = Lexer.push_lexer l in
        match Lexer.lex l with
        | Ok { filename; line; col; token = Lexer.Match } ->
            begin
                match parse_top l with
                | Ok value ->
                    begin
                        match Lexer.lex l with
                        | Ok { token = Lexer.With; _ } ->
                            let rec helper l a =
                                let state = Lexer.push_lexer l in
                                    match Lexer.lex l with
                                    | Ok { token = Lexer.Bar; _ } ->
                                        begin
                                            match parse_pattern l with
                                            | Ok p ->
                                                    begin
                                                        match Lexer.lex l with
                                                        | Ok { token = Lexer.RArrow; _ } ->
                                                            begin
                                                                match parse_top l with
                                                                | Ok v -> helper l ((p, v) :: a)
                                                                | Error e ->
                                                                    Lexer.pop_lexer l state;
                                                                    Error e
                                                            end
                                                        | _ ->
                                                            Lexer.pop_lexer l state;
                                                            Error "invalid match"
                                                    end
                                            | Error e -> Error e                                            
                                        end
                                    | _ ->
                                        Lexer.pop_lexer l state;
                                        Ok a
                            in begin
                                match helper l [] with
                                | Ok matches -> Ok { filename; line; col; ast = Match (value, matches) }
                                | Error e -> Error e                            
                            end
                        | _ ->
                            Lexer.pop_lexer l state;
                            Error "invalid match"
                    end
                | Error e ->
                    Lexer.pop_lexer l state;
                    Error e
            end
        | _ ->
            Lexer.pop_lexer l state;
            Error "invalid match"
*)

and parse_value l =
    (map (consume_token (Lexer.Float 0.)) (function
                                           | { filename; line; col; token = Lexer.Float f } -> { filename; line; col; ast = Float f }
                                           | _ -> raise Exit)
    |* map (consume_token (Lexer.Bool false)) (function
                                               | { filename; line; col; token = Lexer.Bool b } -> { filename; line; col; ast = Bool b }
                                               | _ -> raise Exit)
    |* map (consume_token (Lexer.Symbol "")) (function
                                              | { filename; line; col; token = Lexer.Symbol s } -> { filename; line; col; ast = Symbol s }
                                              | _ -> raise Exit)
    |* (consume_token Lexer.LParen *> parse_top <* consume_token Lexer.RParen)) l

and parse_call l =
    let* v = many_one parse_value l in
        match v with
        | [x]       -> Ok x
        | f :: args -> Ok ({ filename = f.filename; line = f.line; col = f.col; ast = Call (f, args) })
        | _         -> raise Exit

and parse_mult l =
    let rec helper a l =
        match (consume_token Lexer.Star |* consume_token Lexer.Slash |* consume_token Lexer.Percent) l with
        | Ok { token; _ }   ->
            let op = match token with
                     | Lexer.Star    -> Mul
                     | Lexer.Slash   -> Div
                     | Lexer.Percent -> Mod
                     | _             -> raise Exit
            in
            let state = Lexer.push_lexer l in
                begin
                    match parse_call l with
                    | Ok v    -> helper { filename = a.filename; line = a.line; col = a.col; ast = BinOp (op, a, v) } l
                    | Error e ->
                        Lexer.pop_lexer l state;
                        Error e
                end
        | Error _ -> Ok a
    in let* a = parse_call l in
        helper a l

and parse_add l =
    let rec helper a l =
        match (consume_token Lexer.Plus |* consume_token Lexer.Minus) l with
        | Ok { token; _ }   ->
            let op = match token with
                     | Lexer.Plus  -> Add
                     | Lexer.Minus -> Sub
                     | _             -> raise Exit
            in
            let state = Lexer.push_lexer l in
                begin
                    match parse_mult l with
                    | Ok v    -> helper { filename = a.filename; line = a.line; col = a.col; ast = BinOp (op, a, v) } l
                    | Error e ->
                        Lexer.pop_lexer l state;
                        Error e
                end
        | Error _ -> Ok a
    in let* a = parse_mult l in
        helper a l

and parse_cons l =
    let rec helper a l =
        match consume_token Lexer.DoubleColon l with
        | Ok _    ->
            let state = Lexer.push_lexer l in
                begin
                    match parse_add l with
                    | Ok v    -> helper (v :: a) l
                    | Error e ->
                        Lexer.pop_lexer l state;
                        Error e
                end
        | Error _ -> Ok (List.rev a)
    in let* a = parse_add l in
    let* a = helper [a] l in
    let rec helper a =
        match a with
        | [a]       -> a
        | a :: asts -> { filename = a.filename; line = a.line; col = a.col; ast = BinOp (Cons, a, helper asts) }
        | []        -> raise Exit
    in Ok (helper a)

and parse_top l =
    (parse_let |* parse_if |* parse_cons) l;;

let parse filename contents =
    let l = Lexer.create_lexer filename contents
    in parse_top l;;
