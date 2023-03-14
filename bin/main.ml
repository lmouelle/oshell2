open Oshell2.Parseutils
open Oshell2.Executor

let repl _ =
  print_newline ();
  let rec repl' prev_shell_vars =
    try
      print_string "> ";
      let ln = read_line () in
      let prog = parse_string ln in
      let {shell_vars; exitcode = _} = exec prev_shell_vars prog in
      repl' (shell_vars @ prev_shell_vars)
    with
    | End_of_file -> ()
    | NoSuchVar name -> (Printf.printf "No known var named: %s"  name)
  in
  repl' []

let () =
  if Array.length Sys.argv > 2 then (
    Printf.printf "Usage: oshell2 [script]";
    exit 64)
  else if Array.length Sys.argv == 2 then
    failwith "TODO: Impl running files"
  else repl ()
