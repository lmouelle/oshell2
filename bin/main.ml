open Oshell2.Parseutils
open Oshell2.Executor

let repl _ = failwith "TODO"

let runcommand command_string =
  let prog = parse_string command_string in
  let result = Unix.handle_unix_error exec [] prog in
  let lastexitcode = List.assoc "$?" result in
  exit (int_of_string lastexitcode)

let () =
  let len = Array.length Sys.argv in
  if len < 2 then repl ()
  else
    match Sys.argv.(1) with
    | "-f" -> failwith "TODO:impl running files"
    | "-i" -> repl ()
    | "-c" -> runcommand Sys.argv.(2)
    | _ as etc -> failwith ("Unrecognized flag " ^ etc)
