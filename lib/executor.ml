open Ast

(*For now I'm making env a (string * string) list and transorming int values
  like $? and $!  (last exit code and last pid) back and forth from strings into the env.
  Long term I need to get rid of this and replace it with something type safe.

  This means I NEED to remember to set $! and $? every time though. Sucks.

  Also I keep updating the environment all over the place by hand, do I need to do that?

  Also, replace these FATAL FORK FAILURES and all FAILWITHS with proper custom exceptions
*)

let wait_for_result executor_func (env : env) body : env =
  let result = executor_func env body in
  let vars' = result @ env in
  let pid = List.assoc "$!" vars' |> int_of_string in
  let _, status = Unix.waitpid [ Unix.WUNTRACED ] pid in
  match status with
  | Unix.WEXITED exitcode -> ("$?", string_of_int exitcode) :: vars'
  | _ -> failwith "TODO: Stopped and signalled processes unimplemented"

(* TODO redirections and shell variable expansion in here? *)
let exec_command vars cmd = failwith "TODO"

(*TODO: How do we get last exit code/$? in this arrangement?
  Set the pid of the last process/$! and rely on caller to do wait_for_result I guess? *)
let exec_pipe_seq vars seq =
  let temp_stdin = Unix.dup Unix.stdin in
  let pipeline_array = Array.of_list seq in
  let upper_index_bound = Array.length pipeline_array - 1 in
  (* Iterate from 0 to just below max index - 1, performing the final cmd/fork after the loop *)
  for i = 0 to upper_index_bound - 1 do
    let fd_in, fd_out = Unix.pipe () in
    let cmd = pipeline_array.(i) in
    let pid = Unix.fork () in
    if pid < 0 then failwith "FATAL FORK FAILURE"
    else if pid > 0 then (
      Unix.dup2 fd_in Unix.stdin;
      Unix.close fd_out;
      Unix.close fd_in)
    else (
      Unix.dup2 fd_out Unix.stdout;
      Unix.close fd_out;
      Unix.close fd_in;
      exec_command vars cmd)
  done;
  let vars' =
    if Array.length pipeline_array = 0 then vars
    else
      let cmd = pipeline_array.(upper_index_bound) in
      let pid = Unix.fork () in
      if pid < 0 then failwith "FATAL FORK FAILURE"
      else if pid > 0 then ("?!", pid |> string_of_int) :: vars
      else exec_command vars cmd
  in
  Unix.dup2 temp_stdin Unix.stdin;
  vars'

let exec_pipeline vars pipe =
  match pipe with
  | PipelineBangSeq seq ->
      let result = exec_pipe_seq vars seq in
      let exitcode = List.assoc "$?" result |> int_of_string in
      if exitcode <> 0 then ("$?", string_of_int 0) :: result
      else ("$?", string_of_int 1) :: result
  | PipelineSeq seq -> exec_pipe_seq vars seq

let rec exec_conditional vars cond : env =
  match cond with
  | ConditionalPipeline p -> exec_pipeline vars p
  | ConditionalOr (lhs, rhs) ->
      let result = exec_conditional vars lhs in
      let exitcode = List.assoc "$?" result |> int_of_string in
      if exitcode <> 0 then exec_pipeline result rhs else result
  | ConditionalAnd (lhs, rhs) ->
      let result = exec_conditional vars lhs in
      let exitcode = List.assoc "$?" result |> int_of_string in
      if exitcode <> 0 then result else exec_pipeline result rhs

let rec exec_shell_list (vars : env) (lst : shell_list) : env =
  match lst with
  | ShellListConditional cond -> 
      let result = wait_for_result exec_conditional vars cond in
      result @ vars
  | ShellListForeground (lhs, cond) ->
      let lhs_result = wait_for_result exec_shell_list vars lhs in
      wait_for_result exec_conditional lhs_result cond
  | ShellListBackground (lhs, cond) ->
      let lhs_result = exec_shell_list vars lhs in
      wait_for_result exec_conditional lhs_result cond

let exec_complete_command (vars : env) (complete_command : complete_command) =
  match complete_command with
  | CompleteCommandShellList lst | CompleteCommandForeground lst ->
      wait_for_result exec_shell_list vars lst
  | CompleteCommandBackground lst -> exec_shell_list vars lst

let rec exec (vars : env) (prog : program) : env =
  match prog with
  | [] -> vars
  | cmd :: [] ->
      let result = exec_complete_command vars cmd in
      result @ vars
  | cmd :: cmds ->
      let result = exec_complete_command vars cmd in
      let vars' = vars @ result in
      exec vars' cmds
