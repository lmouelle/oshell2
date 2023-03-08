open Oshell2.Parseutils
open Oshell2.Executor

let repl _ =
  print_newline ();
  let rec repl' _ =
    print_string "> ";
    let ln = read_line () in
    let prog = parse_string ln in
    ignore(exec prog);
    repl' ()
  in
  try repl' () with End_of_file -> ()

let () =
  if Array.length Sys.argv > 2 then (
    Printf.printf "Usage: oshell2 [script]";
    exit 64)
  else if Array.length Sys.argv == 2 then
    failwith "TODO: Impl running files"
  else repl ()
