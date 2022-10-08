type token =
    Float of float;;

type lexer = {
    contents: string;
    mutable index: int
};;

let create_lexer contents = { contents; index = 0 };;

type state = Start | FloatTop | FloatPastDot | FloatPastEe | FloatPastEeFirst;;

let lex =
    let rec helper s state l =
        let c = if String.length l.contents <= l.index then
                char_of_int 0
            else l.contents.[l.index]
        in let _ = l.index <- l.index + 1 in
            match state with
            | Start ->
                begin
                    match c with
                    | '0' .. '9'               -> helper (s ^ String.make 1 c) FloatTop l
                    | ' ' | '\n' | '\t' | '\r' -> helper s Start l
                    | _                        -> Error "invalid character"
                end
            | FloatTop ->
                begin
                    match c with
                    | '0' .. '9' -> helper (s ^ String.make 1 c) FloatTop l
                    | '.'        -> helper (s ^ String.make 1 c) FloatPastDot l
                    | 'e' | 'E'  -> helper (s ^ String.make 1 c) FloatPastEe l
                    | _          -> Ok (Float (float_of_string s))
                end
            | FloatPastDot ->
                begin
                    match c with
                    | '0' .. '9' -> helper (s ^ String.make 1 c) FloatPastDot l
                    | 'e' | 'E'  -> helper (s ^ String.make 1 c) FloatPastEe l
                    | _          -> Ok (Float (float_of_string s))
                end
            | FloatPastEe ->
                begin
                    match c with
                    | '0' .. '9' | '+' | '-' -> helper (s ^ String.make 1 c) FloatPastEeFirst l
                    | _                      -> Error "invalid float"
                end
            | FloatPastEeFirst ->
                begin
                    match c with
                    | '0' .. '9' -> helper (s ^ String.make 1 c) FloatPastEeFirst l
                    | _          -> Ok (Float (float_of_string s))
                end
    in helper "" Start;;
