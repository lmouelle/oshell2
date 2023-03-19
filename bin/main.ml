open Oshell2.Parseutils
open Oshell2.Executor

let repl _ =
  print_newline ();
  let rec repl' prev_shell_vars =
    try
      print_string "> ";
      let ln = read_line () in
      let prog = parse_string ln in
      let { shell_vars; exitcode = _ } = exec prev_shell_vars prog in
      repl' (shell_vars @ prev_shell_vars)
    with
    | End_of_file -> ()
    | NoSuchVar name -> Printf.printf "No known var named: %s" name
  in
  Unix.handle_unix_error repl' []

let runcommand command_string =
  let prog = parse_string command_string in
  let { shell_vars = _; exitcode } = Unix.handle_unix_error exec [] prog in
  exit exitcode

let () =
  let len = Array.length Sys.argv in
  if len < 2 then repl ()
  else
    match Sys.argv.(1) with
    | "-f" -> failwith "TODO:impl running files"
    | "-i" -> repl ()
    | "-c" -> runcommand Sys.argv.(2)
    | _ as etc -> failwith ("Unrecognized flag " ^ etc)
