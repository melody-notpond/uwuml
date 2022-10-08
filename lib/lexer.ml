type token_raw =
    Eof
    | Float of float
    | Star
    | Slash
    | Plus
    | Minus
    | Percent
    | DoubleColon
    | LParen
    | RParen;;
type token = { filename: string; line: int; col: int; token: token_raw };;

type lexer = {
    contents: string;
    filename: string;
    mutable index: int;
    mutable line: int;
    mutable col: int;
};;

type lexer_state = {
    index: int;
    line: int;
    col: int;
};;

let create_lexer filename contents = { contents; filename; index = 0; line = 1; col = 0 };;
let push_lexer (l: lexer) = { index = l.index; line = l.line; col = l.col };;
let pop_lexer (l: lexer) state =
    l.index <- state.index;
    l.line <- state.line;
    l.col <- state.col;
    ();;

type state = Start | MultilineComment of int | FloatTop | FloatPastDot | FloatPastEe | FloatPastEeFirst;;

let lex (l: lexer) =
    let rec helper s state line col l =
        let c = if String.length l.contents <= l.index then
                char_of_int 0
            else l.contents.[l.index]
        in let _ = l.index <- l.index + 1 in
        let _ = l.col <- l.col + 1 in
            match state with
            | Start ->
                begin
                    match c with
                    | '0' .. '9'        -> helper (s ^ String.make 1 c) FloatTop line col l
                    | '('               ->
                        if String.length l.contents > l.index && l.contents.[l.index] == '*' then
                            let _ = l.index <- l.index + 1 in
                            helper s (MultilineComment 1) line col l
                        else Ok { filename = l.filename; line; col; token = LParen }
                    | ')'               -> Ok { filename = l.filename; line; col; token = RParen }
                    | '*'               -> Ok { filename = l.filename; line; col; token = Star }
                    | '/'               -> Ok { filename = l.filename; line; col; token = Slash }
                    | '+'               -> Ok { filename = l.filename; line; col; token = Plus }
                    | '-'               -> Ok { filename = l.filename; line; col; token = Minus }
                    | '%'               -> Ok { filename = l.filename; line; col; token = Percent }
                    | ':'               ->
                        if String.length l.contents > l.index && l.contents.[l.index] == ':' then
                            let _ = l.index <- l.index + 1 in
                            Ok { filename = l.filename; line; col; token = DoubleColon }
                        else Error "invalid token"
                    | ' ' | '\t' | '\r' -> helper s Start l.line l.col l
                    | '\n'              ->
                        l.line <- l.line + 1;
                        l.col <- 0;
                        helper s Start l.line l.col l
                    | _                 ->
                        if c == char_of_int 0 then
                            Ok { filename = l.filename; line; col; token = Eof }
                        else Error "invalid character"
                end
            | MultilineComment level ->
                begin
                    match c with
                    | '(' ->
                        if String.length l.contents > l.index && l.contents.[l.index] == '*' then
                            let _ = l.index <- l.index + 1 in
                            helper s (MultilineComment (level + 1)) line col l
                        else helper s (MultilineComment level) line col l
                    | '*' ->
                        if String.length l.contents > l.index && l.contents.[l.index] == ')' then
                            let _ = l.index <- l.index + 1 in
                            if level == 1 then
                                helper s Start l.line l.col l
                            else helper s (MultilineComment (level - 1)) line col l
                        else helper s (MultilineComment level) line col l
                    | '\n' ->
                        l.line <- l.line + 1;
                        l.col <- 0;
                        helper s (MultilineComment level) line col l
                    | _    -> helper s (MultilineComment level) line col l
                end
            | FloatTop ->
                begin
                    match c with
                    | '0' .. '9' -> helper (s ^ String.make 1 c) FloatTop line col l
                    | '.'        -> helper (s ^ String.make 1 c) FloatPastDot line col l
                    | 'e' | 'E'  -> helper (s ^ String.make 1 c) FloatPastEe line col l
                    | _          ->
                        l.index <- l.index - 1;
                        l.col <- l.col - 1;
                        Ok { filename = l.filename; line; col; token = Float (float_of_string s) }
                end
            | FloatPastDot ->
                begin
                    match c with
                    | '0' .. '9' -> helper (s ^ String.make 1 c) FloatPastDot line col l
                    | 'e' | 'E'  -> helper (s ^ String.make 1 c) FloatPastEe line col l
                    | _          ->
                        l.index <- l.index - 1;
                        l.col <- l.col - 1;
                        Ok { filename = l.filename; line; col; token = Float (float_of_string s) }
                end
            | FloatPastEe ->
                begin
                    match c with
                    | '0' .. '9' | '+' | '-' -> helper (s ^ String.make 1 c) FloatPastEeFirst line col l
                    | _                      -> Error "invalid float"
                end
            | FloatPastEeFirst ->
                begin
                    match c with
                    | '0' .. '9' -> helper (s ^ String.make 1 c) FloatPastEeFirst line col l
                    | _          ->
                        l.index <- l.index - 1;
                        l.col <- l.col - 1;
                        Ok { filename = l.filename; line; col; token = Float (float_of_string s) }
                end
    in helper "" Start l.line l.col l;;

