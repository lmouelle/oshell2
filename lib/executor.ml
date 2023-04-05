open Ast

type exec_result = { exitcode : int; shell_vars : variable_entry list }

exception ExecError of string
exception NoSuchVar of string

let lastexitcode = ref 0

let redirect { file_desc = fd; filename; _ } =
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

let rec resolve_var shell_vars varname =
  if String.starts_with ~prefix:"$" varname then
    match List.assoc_opt varname shell_vars with
    | None -> raise @@ NoSuchVar varname
    | Some (Exitcode i) -> string_of_int i
    | Some (String s) -> s
    | Some (Variable v) -> resolve_var shell_vars v
  else varname

let resolve_vars shell_vars { executable; variables; args; redirections } =
  let shell_vars' = variables @ shell_vars in
  let executable' = resolve_var shell_vars' executable in
  let args' = List.map (resolve_var shell_vars') args in
  { executable = executable'; variables; redirections; args = args' }

let exec_pipeline shell_vars pipeline wait : exec_result =
  let tempin = Unix.dup Unix.stdin in
  let pipeline_array = Array.of_list pipeline in
  let accumulated_shell_vars = ref shell_vars in
  let upper_index_bound = Array.length pipeline_array - 1 in
  (* Iterate from 0 to just max index - 1, performing the final fork after this loop*)
  for i = 0 to upper_index_bound - 1 do
    let fd_in, fd_out = Unix.pipe () in
    let command = pipeline_array.(i) |> resolve_vars !accumulated_shell_vars in
    accumulated_shell_vars := command.variables @ !accumulated_shell_vars;
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
      if command.executable <> String.empty then
        Unix.execvp command.executable
          (Array.of_list (command.executable :: command.args))
      else exit 0)
  done;

  (* TODO: Handle empty input from the user as a no-op.
     Not very satisfied with this, find alternative in parser *)
  if Array.length pipeline_array = 0 then
    {
      exitcode = !lastexitcode;
      shell_vars =
        ("$?", String (string_of_int !lastexitcode)) :: !accumulated_shell_vars;
    }
  else
    let command =
      pipeline_array.(upper_index_bound) |> resolve_vars shell_vars
    in
    accumulated_shell_vars := command.variables @ !accumulated_shell_vars;
    let pid = Unix.fork () in
    if pid < 0 then
      raise
      @@ ExecError ("Fork failed for command: " ^ command_to_string command)
    else if pid > 0 then
      if wait then
        let _, status = Unix.waitpid [ Unix.WUNTRACED ] pid in
        match status with
        | Unix.WEXITED exitcode -> lastexitcode := exitcode
        | _ -> failwith "TODO: Stopped and signalled processes unimplemented"
      else ()
    else (
      List.iter redirect command.redirections;
      if command.executable <> String.empty then
        Unix.execvp command.executable
          (Array.of_list (command.executable :: command.args))
      else exit 0);

    Unix.dup2 tempin Unix.stdin;
    {
      exitcode = !lastexitcode;
      shell_vars =
        ("$?", String (string_of_int !lastexitcode)) :: !accumulated_shell_vars;
    }

let rec exec_conditional shell_vars wait = function
  | BasePipeline p -> exec_pipeline shell_vars p wait
  | Or (lhs, rhs) ->
      let ({ exitcode; shell_vars } as result) =
        exec_conditional shell_vars wait lhs
      in
      if exitcode <> 0 then exec_conditional shell_vars wait rhs else result
  | And (lhs, rhs) ->
      let ({ exitcode; shell_vars } as result) =
        exec_conditional shell_vars wait lhs
      in
      if exitcode <> 0 then result else exec_conditional shell_vars wait rhs

let rec exec_shell_list shell_vars wait = function
  | BaseConditional c -> exec_conditional shell_vars wait c
  | ShellListBackground (lhs, rhs) -> 
    let {shell_vars = shell_vars'; exitcode = _} = exec_shell_list shell_vars false lhs in
    exec_shell_list shell_vars' wait rhs
  | ShellListForeground (lhs, rhs) -> 
    let {shell_vars = shell_vars'; exitcode = _} = exec_shell_list shell_vars true lhs in
    exec_shell_list shell_vars' wait rhs

let exec shell_vars = function
  | BaseProgram p -> exec_shell_list shell_vars true p
  | ProgramBackground p -> exec_shell_list shell_vars false p
  | ProgramForeground p -> exec_shell_list shell_vars true p
