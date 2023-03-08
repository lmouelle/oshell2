open Ast

exception ExecError of string

let lastexitcode = ref 0

let redirect {file_desc = fd; filename; _} =
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

let exec_pipeline pipeline =
  let pipeline_array = Array.of_list pipeline in
  let upper_index_bound = Array.length pipeline_array - 1 in
  (* Iterate from 0 to just max index - 1, performing the final fork after this loop*)
  for i = 0 to upper_index_bound - 1 do
    let fd_in, fd_out = Unix.pipe () in
    let command = pipeline_array.(i) in
    let pid = Unix.fork () in
    if pid < 0 then
      raise
      @@ ExecError ("Fork failed for command: " ^ command_to_string command)
    else if pid > 0 then (
      Unix.dup2 fd_in Unix.stdin;
      Unix.close fd_out;
      Unix.close fd_in)
    else (
      Unix.dup2 fd_out Unix.stdout;
      Unix.close fd_out;
      Unix.close fd_in;
      List.iter redirect command.redirections;
      Unix.execvp command.executable
        (Array.of_list (command.executable :: command.args)))
  done;

  let command = pipeline_array.(upper_index_bound) in
  let pid = Unix.fork () in
  if pid < 0 then
    raise @@ ExecError ("Fork failed for command: " ^ command_to_string command)
  else if pid > 0 then
    let _, status = Unix.waitpid [ Unix.WUNTRACED ] pid in
    match status with
    | Unix.WEXITED exitcode -> lastexitcode := exitcode
    | _ -> failwith "TODO: Stopped and signalled processes unimplemented"
  else (
    List.iter redirect command.redirections;
    Unix.execvp command.executable
      (Array.of_list (command.executable :: command.args)));
  
  !lastexitcode

let rec exec_conditional = function
| BasePipeline p -> exec_pipeline p
| Or (lhs, rhs) ->
  let retcode = exec_conditional lhs in
  if retcode <> 0 then exec_conditional rhs else retcode
| And (lhs, rhs) ->
  let retcode = exec_conditional lhs in
  if retcode <> 0 then retcode else exec_conditional rhs