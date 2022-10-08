type token =
    Float of float;;
type lexer;;

val create_lexer : string -> lexer;;
val lex : lexer -> (token, string) result;;
