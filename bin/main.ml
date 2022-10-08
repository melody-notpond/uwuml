let filename = Sys.argv.(1);;
let file = open_in filename;;
let rec read_file file contents =
    try
        read_file file (contents ^ input_line file)
    with _ ->
        close_in_noerr file;
        contents;;
let contents = read_file file "";;

open Uwuml;;

let a = match Parse.parse filename contents with
    | Ok v -> v
    | Error e ->
        Printf.eprintf "error encountered: %s" e;
        exit 1;;
Parse.print_ast a;;

(*
things in uwuml:
 - lists
 - booleans
 - adts
 - integers
 - tuples
 - strings
 - top type
 - nil type
 - let (rec) ... = ...
 - let (rec) ... = ... in ...
 - match ... with ... -> ... else with ... -> ... else -> ...
 - if ... then ... else ...
 - generics
 - basic typeclasses
 - type inference

 *)
