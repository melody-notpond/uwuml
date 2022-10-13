type bin_op = Mul | Div | Add | Sub | Mod | Cons;;
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

val print_ast : ast -> unit;;
val print_type : ty -> unit;;
val parse : string -> string -> (ast list, string) result;;

