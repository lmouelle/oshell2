open Ast
open Env

(*
  I keep updating the environment all over the place by hand, do I need to do that?
  TODO: Replace these FATAL FORK FAILURES and all FAILWITHS with proper custom exceptions
*)

let wait_for_result executor_func (env : env) body : env =
  let result = executor_func env body in
  let pid = get_last_pid result in
  let _, status = Unix.waitpid [ Unix.WUNTRACED ] pid in
  match status with
  | Unix.WEXITED exitcode -> update_last_exitcode result exitcode
  | _ -> failwith "TODO: Stopped and signalled processes unimplemented"

let exec_redirection { io_num; filename } =
  match io_num with
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
  | _ -> failwith "TODO: Impl arbitrary file descriptor redirection"

let rec exec_simple_command vars cmd =
  List.iter exec_redirection cmd.redirections;
  let executable = Option.map (resolve_var vars) cmd.name in
  let args = List.map (resolve_var vars) cmd.args in
  match executable with
  | None -> vars
  | Some executable ->
      Unix.execvp executable (Array.of_list (executable :: args))

(* TODO redirections and shell variable expansion in here? *)
and exec_command (vars : env) cmd =
  match cmd with
  | CommandSimpleCommand simple_cmd -> exec_simple_command vars simple_cmd
  | CommandCompoundCommand (compound_cmd, redirs) ->
      let result = exec_compound_command vars compound_cmd in
      List.iter exec_redirection redirs;
      result

(*TODO: How do we get last exit code/$? in this arrangement?
  Set the pid of the last process/$! and rely on caller to do wait_for_result I guess? *)
and exec_pipe_seq (vars : env) seq =
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
      ignore (exec_command vars cmd))
  done;
  let vars' =
    if Array.length pipeline_array = 0 then vars
    else
      let cmd = pipeline_array.(upper_index_bound) in
      let pid = Unix.fork () in
      if pid < 0 then failwith "FATAL FORK FAILURE"
      else if pid > 0 then update_last_pid vars pid
      else exec_command vars cmd
  in
  Unix.dup2 temp_stdin Unix.stdin;
  vars'

and exec_pipeline vars pipe =
  match pipe with
  | PipelineBangSeq seq ->
      let result = exec_pipe_seq vars seq in
      let exitcode = get_last_exitcode result in
      if exitcode <> 0 then update_last_exitcode result 0
      else update_last_exitcode result 1
  | PipelineSeq seq -> exec_pipe_seq vars seq

and exec_conditional vars cond : env =
  match cond with
  | ConditionalPipeline p -> exec_pipeline vars p
  | ConditionalOr (lhs, rhs) ->
      let result = exec_conditional vars lhs in
      let exitcode = get_last_exitcode result in
      if exitcode <> 0 then exec_pipeline result rhs else result
  | ConditionalAnd (lhs, rhs) ->
      let result = exec_conditional vars lhs in
      let exitcode = get_last_exitcode result in
      if exitcode <> 0 then result else exec_pipeline result rhs

and exec_term vars = function
  | TermConditional cond -> wait_for_result exec_conditional vars cond
  | TermForeground (lhs, cond) ->
      let lhs_result = wait_for_result exec_term vars lhs in
      wait_for_result exec_conditional lhs_result cond
  | TermBackground (lhs, cond) ->
      let lhs_result = exec_term vars lhs in
      wait_for_result exec_conditional lhs_result cond

and exec_compound_list vars = function
  | CompoundListForeground t | CompoundListTerm t ->
      wait_for_result exec_term vars t
  | CompoundListBackground t -> exec_term vars t

and exec_compound_command vars = function
  | CompoundCommandIf { ifelse; tests } ->
      let rec exec_if_tests = function
        | [] ->
            (* Must always have at least initial if statement, list of 1 - n length*)
            failwith "MALFORMED IF COMMAND"
        | { test; body } :: [] -> (
            let test_result = exec_compound_list vars test in
            if get_last_exitcode test_result = 0 then
              exec_compound_list vars body
            else
              match ifelse with
              | None -> vars
              | Some final_body -> exec_compound_list vars final_body)
        | { test; body } :: remainder ->
            let test_result = exec_compound_list vars test in
            if get_last_exitcode test_result = 0 then
              exec_compound_list vars body
            else exec_if_tests remainder
      in
      let first_success_env = exec_if_tests tests in
      if get_last_exitcode first_success_env <> 0 then
        match ifelse with
        | None -> vars
        | Some clist -> exec_compound_list vars clist
      else vars
  | CompoundCommandWhile { test; body } ->
      let rec eval_while prev_result =
        let test_result = exec_compound_list vars test in
        if get_last_exitcode test_result = 0 then
          let body_result = exec_compound_list vars body in
          eval_while body_result
        else prev_result
      in
      eval_while vars

and exec_shell_list (vars : env) (lst : shell_list) : env =
  match lst with
  | ShellListConditional cond -> wait_for_result exec_conditional vars cond
  | ShellListForeground (lhs, cond) ->
      let lhs_result = wait_for_result exec_shell_list vars lhs in
      wait_for_result exec_conditional lhs_result cond
  | ShellListBackground (lhs, cond) ->
      let lhs_result = exec_shell_list vars lhs in
      wait_for_result exec_conditional lhs_result cond

and exec_complete_command (vars : env) (complete_command : complete_command) =
  match complete_command with
  | CompleteCommandShellList lst | CompleteCommandForeground lst ->
      wait_for_result exec_shell_list vars lst
  | CompleteCommandBackground lst -> exec_shell_list vars lst

and exec (vars : env) (prog : program) : env =
  match prog with
  | [] -> vars
  | cmd :: [] -> exec_complete_command vars cmd
  | cmd :: cmds ->
      let result = exec_complete_command vars cmd in
      let vars' = merge_envs result vars in
      exec vars' cmds
