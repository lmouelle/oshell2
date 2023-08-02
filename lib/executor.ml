open Ast

(*For now I'm making env a (string * string) list and transorming int values
  like $? and $!  (last exit code and last pid) back and forth from strings into the env.
  Long term I need to get rid of this and replace it with something type safe.

  This means I NEED to remember to set $! and $? every time though. Sucks.

  Also I keep updating the environment all over the place by hand, do I need to do that?
*)

let wait_for_result extractor (env : env) body : env =
  let result = extractor env body in
  let vars' = result @ env in
  let pid = List.assoc "$!" vars' |> int_of_string in
  let _, status = Unix.waitpid [ Unix.WUNTRACED ] pid in
  match status with
  | Unix.WEXITED exitcode -> ("$?", string_of_int exitcode) :: vars'
  | _ -> failwith "TODO: Stopped and signalled processes unimplemented"

let exec_pipeline _vars _cond = failwith "TODO"

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
  | ShellListConditional cond -> exec_conditional vars cond
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
