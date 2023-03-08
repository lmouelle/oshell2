open Oshell2.Parseutils
open Oshell2.Executor

let repl _ =
  print_newline ();
  let rec repl' _ =
    print_string "> ";
    let ln = read_line () in
    (* TODO: Implement variables properly and just make last exit code a case of variables *)
    if (String.trim ln = "$?") 
    then Printf.printf "%d\n" (get_last_exit_code ()) 
    else 
      let prog = parse_string ln in
      exec_pipeline prog;
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
