(* type bin_op;; *)
type ast_raw =
    | Float of float
    (* | BinOp of bin_op * ast * ast *)
and ast = { filename: string; line: int; col: int; ast: ast_raw };;

val parse : string -> string -> (ast, string) result;;
val print_ast : ast -> unit;;

