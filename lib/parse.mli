(* type bin_op;; *)
type ast_raw;;
type ast;;

val parse : string -> string -> (ast, string) result;;
val print_ast : ast -> unit;;

