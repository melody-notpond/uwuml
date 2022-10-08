type token_raw =
    Eof
    | Float of float;;
type token = { filename: string; line: int; col: int; token: token_raw };;
type lexer;;

val create_lexer : string -> string -> lexer;;
val lex : lexer -> (token, string) result;;
