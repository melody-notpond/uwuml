type bin_op = Mul | Div | Add | Sub | Mod | Cons | Lt | Gt | Eq | Ne | Le | Ge;;
type pattern_raw =
    | Wildcard
    | SymbolPat of string
    | FloatPat of float
    | BoolPat of bool
    | ConsPat of pattern * pattern
    | SumPat of string * pattern list
    | ManyPat of pattern * pattern
and pattern = { filename: string; line: int; col: int; pattern: pattern_raw };;
type ty =
    | Unknown
    | TypeVar of int
    | Generic of string
    | TypeName of string * ty list
    | Product of ty list
    | Function of ty * ty;;
type ast_raw =
    | Float of float
    | Bool of bool
    | Symbol of string
    | BinOp of bin_op * ast * ast
    | Call of ast * ast list
    | Let of bool * string * string list * ast * ast option
    | If of ast * ast * ast
    | Match of ast * (pattern * ast) list
    | TypeSumDef of string * (string * ty option) list
    | TypeDef of string * ty
    | Many of ast list
and ast = { filename: string; line: int; col: int; mutable ty: ty; ast: ast_raw };;

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
        print_string ")"
    | FloatPat f ->
        print_float f
    | BoolPat b ->
        print_string (if b then "true" else "false");;

let rec print_type t =
    match t with
    | Unknown -> print_string "<unknown>"
    | TypeVar var -> Printf.printf "$%i" var
    | Generic g -> Printf.printf "'%s" g
    | TypeName (name, types) ->
        print_string "(";
        print_string name;
        List.iter (fun ty -> print_string " "; print_type ty) types;
        print_string ")"
    | Product types ->
        print_string "(";
        begin
            match types with
            | t :: ts ->
                print_type t;
                List.iter (fun ty -> print_string " * "; print_type ty) ts
            | [] -> ()
        end;
        print_string ")"
    | Function (arg, ret) ->
        print_string "(";
        print_type arg;
        print_string " -> ";
        print_type ret;
        print_string ")";;

let print_ast =
    let rec helper indentation a =
        for _ = 1 to indentation do
            print_string "    ";
        done;
        Printf.printf "%s:%i:%i " a.filename a.line a.col;
        print_type a.ty;
        print_string " : ";
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
                | Lt -> print_string "<"
                | Gt -> print_string ">"
                | Eq -> print_string "="
                | Ne -> print_string "<>"
                | Le -> print_string ">="
                | Ge -> print_string "<="
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
                       | (p, v) -> print_pattern p; print_newline (); helper (indentation + 1) v) pats
        | TypeSumDef (name, variants) ->
            Printf.printf "type %s = \n" name;
            List.iter (function
                       | (variant, Some ty) -> Printf.printf "    | %s of " variant; print_type ty; print_newline ()
                       | (variant, None)    -> Printf.printf "    | %s\n"   variant) variants;
        | TypeDef (name, ty) ->
            Printf.printf "type %s = " name;
            print_type ty;
            print_newline ();
        | Many v ->
            print_string "many\n";
            List.iter (helper (indentation + 1)) v
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
        | (Ok ({ token = Lexer.Generic _; _ } as tok), Lexer.Generic _) -> Ok tok
        | _ ->
            Lexer.pop_lexer l state;
            Error "mismatched token";;

let consume_token_exact t l =
    let state = Lexer.push_lexer l in
        match Lexer.lex l with
        | Ok ({ token; _ } as tok) when token == t -> Ok tok
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

let rec parse_pattern_value l =
    (map (consume_token_exact (Lexer.Symbol "_"))
        (function
         | { filename; line; col; _ } -> { filename; line; col; pattern = Wildcard })
    |* map (consume_token (Lexer.Symbol ""))
        (function
         | { filename; line; col; token = Lexer.Symbol s } -> { filename; line; col; pattern = SymbolPat s }
         | _ -> raise Exit)
    |* map (consume_token (Lexer.Float 0.))
        (function
         | { filename; line; col; token = Lexer.Float f } -> { filename; line; col; pattern = FloatPat f }
         | _ -> raise Exit)
    |* map (consume_token (Lexer.Bool false))
        (function
         | { filename; line; col; token = Lexer.Bool b } -> { filename; line; col; pattern = BoolPat b }
         | _ -> raise Exit)
    |* (consume_token Lexer.LParen *> parse_pattern <* consume_token Lexer.RParen))
        l

and parse_pattern_sum l =
    map (consume_token (Lexer.Symbol "") &* many_one parse_pattern_value)
        (function
         | ({ filename; line; col; token = Lexer.Symbol variant }, xs) ->
            { filename; line; col; pattern = SumPat (variant, xs) }
         | _ -> raise Exit)
        l

and parse_pattern_sum_or_value l =
    (parse_pattern_sum |* parse_pattern_value) l

and parse_pattern_cons l =
    let rec helper p l =
        match consume_token Lexer.DoubleColon l with
        | Ok _    ->
            let state = Lexer.push_lexer l in
                begin
                    match parse_pattern_sum_or_value l with
                    | Ok v    -> helper (v :: p) l
                    | Error e ->
                        Lexer.pop_lexer l state;
                        Error e
                end
        | Error _ -> Ok (List.rev p)
    in let* p = parse_pattern_sum_or_value l in
    let* p = helper [p] l in
    let rec helper (p: pattern list) =
        match p with
        | [p]     -> p
        | p :: ps -> { filename = p.filename; line = p.line; col = p.col; pattern = ConsPat (p, helper ps) }
        | []      -> raise Exit
    in Ok (helper p)

and parse_pattern_many l =
    let rec helper (p: pattern) l =
        match consume_token Lexer.Bar l with
        | Ok _   ->
            let state = Lexer.push_lexer l in
                begin
                    match parse_pattern_cons l with
                    | Ok v    -> helper { filename = p.filename; line = p.line; col = p.col; pattern = ManyPat (p, v) } l
                    | Error e ->
                        Lexer.pop_lexer l state;
                        Error e
                end
        | Error _ -> Ok p
    in let* a = parse_pattern_cons l in
        helper a l

and parse_pattern l = parse_pattern_many l;;

let map_helper (v: Lexer.token) =
    match v.token with
    | Lexer.Symbol s -> s
    | _ -> raise Exit;;

let rec parse_let requires_in l =
    map ((((consume_token Lexer.Let &* optional (consume_token Lexer.Rec) &* consume_token (Lexer.Symbol "") &* many_zero (consume_token (Lexer.Symbol ""))) <* consume_token Lexer.EqualSign) &* parse_top) &* (if requires_in then map (consume_token Lexer.In *> parse_top) (fun x -> Some x) else optional (consume_token Lexer.In *> parse_top)))
        (function
         | ((((({ filename; line; col; _ }, recursive), { token = Lexer.Symbol var; _ }), args), value), context) ->
            { filename; line; col; ty = Unknown; ast = Let (Option.is_some recursive, var, List.map map_helper args, value, context) }
         | _ -> raise Exit)
        l

and parse_if l =
    map (consume_token Lexer.If &* parse_top &* (consume_token Lexer.Then *> parse_top) &* (consume_token Lexer.Else *> parse_top))
        (function
            | ((({ filename; line; col; _ }, cond), theny), elsy) -> { filename; line; col; ty = Unknown; ast = If (cond, theny, elsy) })
        l

and parse_match l =
    map (consume_token Lexer.Match &* (parse_top <* consume_token Lexer.With) &* many_one ((consume_token Lexer.Bar *> parse_pattern) &* (consume_token Lexer.RArrow *> parse_top)))
        (function
         | (({ filename; line; col; _ }, value), branches) -> { filename; line; col; ty = Unknown; ast = Match (value, branches) })
        l

and parse_value l =
    (map (consume_token (Lexer.Float 0.)) (function
                                           | { filename; line; col; token = Lexer.Float f } -> { filename; line; col; ty = Unknown; ast = Float f }
                                           | _ -> raise Exit)
    |* map (consume_token (Lexer.Bool false)) (function
                                               | { filename; line; col; token = Lexer.Bool b } -> { filename; line; col; ty = Unknown; ast = Bool b }
                                               | _ -> raise Exit)
    |* map (consume_token (Lexer.Symbol "")) (function
                                              | { filename; line; col; token = Lexer.Symbol s } -> { filename; line; col; ty = Unknown; ast = Symbol s }
                                              | _ -> raise Exit)
    |* (consume_token Lexer.LParen *> parse_top <* consume_token Lexer.RParen)) l

and parse_call l =
    let* v = many_one parse_value l in
        match v with
        | [x]       -> Ok x
        | f :: args -> Ok ({ filename = f.filename; line = f.line; col = f.col; ty = Unknown; ast = Call (f, args) })
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
                    | Ok v    -> helper { filename = a.filename; line = a.line; col = a.col; ty = Unknown; ast = BinOp (op, a, v) } l
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
                    | Ok v    -> helper { filename = a.filename; line = a.line; col = a.col; ty = Unknown; ast = BinOp (op, a, v) } l
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
        | a :: asts -> { filename = a.filename; line = a.line; col = a.col; ty = Unknown; ast = BinOp (Cons, a, helper asts) }
        | []        -> raise Exit
    in Ok (helper a)

and parse_compare l =
    let rec helper a l =
        match (consume_token Lexer.EqualSign |* consume_token Lexer.Diamond |* consume_token Lexer.LAngle |* consume_token Lexer.RAngle |* consume_token Lexer.LEqual |* consume_token Lexer.GEqual) l with
        | Ok { token; _ }   ->
            let op = match token with
                     | Lexer.EqualSign -> Eq
                     | Lexer.Diamond   -> Ne
                     | Lexer.LAngle    -> Lt
                     | Lexer.RAngle    -> Gt
                     | Lexer.LEqual    -> Le
                     | Lexer.GEqual    -> Ge
                     | _             -> raise Exit
            in
            let state = Lexer.push_lexer l in
                begin
                    match parse_cons l with
                    | Ok v    -> helper { filename = a.filename; line = a.line; col = a.col; ty = Unknown; ast = BinOp (op, a, v) } l
                    | Error e ->
                        Lexer.pop_lexer l state;
                        Error e
                end
        | Error _ -> Ok a
    in let* a = parse_cons l in
        helper a l

and parse_top_value l =
    (parse_let true |* parse_if |* parse_match |* parse_compare) l

and parse_top l =
    let* x = parse_top_value l in
    let* xs = (many_zero (consume_token Lexer.Semicolon *> parse_top_value) <* optional (consume_token Lexer.Semicolon)) l in
        match xs with
        | [] -> Ok x
        | _ ->
            Ok { filename = x.filename; line = x.line; col = x.col; ty = Unknown; ast = Many (x :: xs) };;

let rec parse_type_value l =
    ((map (consume_token (Lexer.Generic "") |* consume_token (Lexer.Symbol ""))
        (function
         | { token = Lexer.Generic g; _ } -> Generic g
         | { token = Lexer.Symbol s; _ }  -> TypeName (s, [])
         | _                              -> raise Exit))
    |* (consume_token (Lexer.LParen) *> parse_type <* consume_token (Lexer.RParen)))
        l

and parse_type_applied l =
    let* name = consume_token (Lexer.Symbol "") l in
    let* args = many_zero parse_type_value l in
        match name with
        | { token = Lexer.Symbol name; _ } -> Ok (TypeName (name, args))
        | _ -> raise Exit

and parse_type_sub l =
    (parse_type_applied |* parse_type_value) l

and parse_type l =
    map (parse_type_sub &* many_zero (consume_token Lexer.Star *> parse_type_sub))
        (function
         | (x, []) -> x
         | (x, xs) -> Product (x :: xs))
        l;;

let parse_sum_type_def_variant l =
    ((consume_token Lexer.Bar *> (map (consume_token (Lexer.Symbol ""))
        (function
         | { token = Lexer.Symbol s; _ } -> s
         | _ -> raise Exit))) &* optional (consume_token Lexer.Of *> parse_type)) l;;

let parse_sum_type_def l =
    map (consume_token Lexer.Type &* consume_token (Lexer.Symbol "") &* (consume_token Lexer.EqualSign *> many_one parse_sum_type_def_variant))
        (function
         | (({ filename; line; col; _ }, { token = Symbol name; _ }), variants) -> { filename; line; col; ty = Unknown; ast = TypeSumDef (name, variants) }
         | _ -> raise Exit)
        l;;

let parse_type_def l =
    (parse_sum_type_def |* (map (consume_token Lexer.Type &* consume_token (Lexer.Symbol "") &* (consume_token Lexer.EqualSign *> parse_type))
        (function
         | (({ filename; line; col; _ }, { token = Lexer.Symbol name; _ }), ty) -> { filename; line; col; ty = Unknown; ast = TypeDef (name, ty)}
         | _ -> raise Exit)))
        l;;

let parse_top_level l =
    (parse_type_def |* parse_top |* parse_let false) l;;

let parse filename contents =
    let l = Lexer.create_lexer filename contents in
    let* x = parse_top_level l in
    let* xs = (many_zero (consume_token Lexer.DoubleSemicolon *> parse_top_level) <* optional (consume_token Lexer.DoubleSemicolon) <* consume_token Lexer.Eof) l in
        Ok (x :: xs)
