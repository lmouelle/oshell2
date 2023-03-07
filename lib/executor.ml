open Ast

exception ExecError of string

let exec_command { executable; args; redirections } =
  let dupfile (fd, filename) =
    match fd with
    | 0 ->
        let filehandle = Unix.openfile filename [ O_RDONLY ] 0o640 in
        Unix.dup2 filehandle Unix.stdin
    | 1 ->
        let filehandle =
          Unix.openfile filename [ O_TRUNC; O_CREAT; O_WRONLY ] 0o640
        in
        Unix.dup2 filehandle Unix.stdout
    | 2 ->
        let filehandle =
          Unix.openfile filename [ O_TRUNC; O_CREAT; O_WRONLY ] 0o640
        in
        Unix.dup2 filehandle Unix.stderr
    | _ -> raise @@ ExecError "TODO: Impl arbitrary file descriptor redirection"
  in
  let pid = Unix.fork () in

  if pid < 0 then raise @@ ExecError "Fork failure"
  else if pid = 0 then (
    List.iter dupfile redirections;
    Unix.execv executable (Array.of_list (executable :: args)))
  else
    let _, status = Unix.waitpid [ Unix.WUNTRACED ] pid in
    match status with
    | Unix.WEXITED exitcode -> exitcode
    | _ -> failwith "Stopped and signalled processes unimplemented"