open Oshell2.Parseutils
open Oshell2.Executor

let lastexitcode = ref (-1)

let exec str =
  let command = parse_string str in
  exec_command command

let repl _ =
  print_newline ();
  let rec repl' _ =
    print_string "> ";
    let ln = read_line () in
    lastexitcode := exec ln;
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
