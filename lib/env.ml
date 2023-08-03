type variable_entry = String of string | Variable of string
type env = (string * variable_entry) list

let add_var env name value =
  let entry =
    if String.starts_with ~prefix:"$" value then Variable value
    else String value
  in
  (name, entry) :: env

let rec resolve_var env name =
  if not @@ String.starts_with ~prefix:"$" name then name
  else
    match List.assoc_opt name env with
    | None -> String.empty
    | Some (String s) -> s
    | Some (Variable v) -> resolve_var env v

let get_last_pid env =
  let entry = List.assoc "$!" env in
  match entry with
  | String string -> int_of_string string
  | Variable _ -> failwith "FATAL MALFORMED VAR"

let get_last_exitcode env =
  let entry = List.assoc "$?" env in
  match entry with
  | String string -> int_of_string string
  | Variable _ -> failwith "FATAL MALFORMED VAR"

let merge_envs lhs rhs = lhs @ rhs
let empty = []
let update_last_exitcode env code = ("$?", String (string_of_int code)) :: env
let update_last_pid env pid = ("$!", String (string_of_int pid)) :: env
