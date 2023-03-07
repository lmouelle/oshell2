open Oshell2.Parseutils
open Oshell2.Executor

let exec str =
  let command = parse_string str in
  exec_command command

let repl _ =
  print_newline ();
  let rec repl' _ =
    print_string "> ";
    let ln = read_line () in
    if (String.trim ln = "$?") then Printf.printf "%d\n" (get_last_exit_code ()) else exec ln;
    repl' ()
  in
  try repl' () with End_of_file -> ()

let () =
  if Array.length Sys.argv > 2 then (
    Printf.printf "Usage: oshell [script]";
    exit 64)
  else if Array.length Sys.argv == 2 then
    failwith "TODO: Impl running files"
  else repl ()
