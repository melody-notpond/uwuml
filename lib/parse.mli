type bin_op = Mul | Div | Add | Sub | Mod | Cons;;
type ast_raw =
    | Float of float
    | BinOp of bin_op * ast * ast
and ast = { filename: string; line: int; col: int; ast: ast_raw };;

val print_ast : ast -> unit;;
val parse : string -> string -> (ast, string) result;;

