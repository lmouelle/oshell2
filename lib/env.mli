type env

val get_last_exitcode : env -> int
val get_last_pid : env -> int
val update_last_pid : env -> int -> env
val update_last_exitcode : env -> int -> env
val resolve_var : env -> string -> string
val add_var : env -> string -> string -> env
val merge_envs : env -> env -> env
val empty : env
