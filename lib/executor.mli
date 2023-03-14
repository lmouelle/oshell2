open Ast

type exec_result = { exitcode : int; shell_vars : variable_entry list }

(** Run a program, returning the last exit code of the program *)
val exec : variable_entry list -> program -> exec_result
