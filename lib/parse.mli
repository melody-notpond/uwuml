type bin_op = Mul | Div | Add | Sub | Mod | Cons;;
type ast_raw =
    | Float of float
    | Symbol of string
    | BinOp of bin_op * ast * ast
    | Call of ast * ast list
    | Let of bool * string * string list * ast * ast option
and ast = { filename: string; line: int; col: int; ast: ast_raw };;

val print_ast : ast -> unit;;
val parse : string -> string -> (ast, string) result;;

