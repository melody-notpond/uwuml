type token_raw =
    Eof
    | Float of float
    | Bool of bool
    | Symbol of string
    | Star
    | Slash
    | Plus
    | Minus
    | Percent
    | DoubleColon
    | LParen
    | RParen
    | Let
    | Rec
    | In
    | EqualSign
    | If
    | Then
    | Else
    | Match
    | With
    | Bar
    | RArrow;;
type token = { filename: string; line: int; col: int; token: token_raw };;
type lexer;;
type lexer_state;;

val create_lexer : string -> string -> lexer;;
val push_lexer : lexer -> lexer_state;;
val pop_lexer : lexer -> lexer_state -> unit;;
val lex : lexer -> (token, string) result;;
val peek : lexer -> (token, string) result;;
