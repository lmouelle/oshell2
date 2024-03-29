type redirection_type = Input | Output

type redirection = {
  redirection_type : redirection_type;
  filename : string;
  file_desc : int;
}

type variable_type = String of string | Variable of string | Exitcode of int
type variable_entry = string * variable_type

type command = {
  executable : string;
  args : string list;
  redirections : redirection list;
  variables : variable_entry list;
}

type pipeline = command list

type conditional =
  | BasePipeline of pipeline
  | And of conditional * conditional
  | Or of conditional * conditional

type shell_list = 
| BaseConditional of conditional
| ShellListForeground of shell_list * shell_list
| ShellListBackground of shell_list * shell_list

(* TODO: Update the program typedef each time I add a new grammar feature.
   Requires updating eveything everywhere else. So tedious. Perhaps investigate using modules
   for encapsulation? *)
type program = 
| BaseProgram of shell_list
| ProgramForeground of shell_list
| ProgramBackground of shell_list

let variable_to_string (varname, varval) =
  let resolvedval =
    match varval with
    | String s -> s
    | Variable v -> "$" ^ v
    | Exitcode i -> string_of_int i
  in
  varname ^ "=" ^ resolvedval

let shell_vars_to_string shell_vars =
  List.map variable_to_string shell_vars |> String.concat ","

let redirection_to_string { filename; file_desc = fd; redirection_type } =
  let seperator = match redirection_type with Input -> "<" | Output -> ">" in
  string_of_int fd ^ seperator ^ filename

let command_to_string { executable; args; redirections; variables } =
  let bare_string =
    [
      executable;
      String.concat " " args;
      List.map redirection_to_string redirections |> String.concat ",";
      List.map variable_to_string variables |> String.concat ",";
    ]
    |> String.concat ";"
  in
  "{" ^ bare_string ^ "}"

let pipeline_to_string pipeline =
  List.map command_to_string pipeline |> String.concat "|"

let rec conditional_to_string = function
  | BasePipeline p -> pipeline_to_string p
  | And (lhs, rhs) ->
      conditional_to_string lhs ^ "&&" ^ conditional_to_string rhs
  | Or (lhs, rhs) ->
      conditional_to_string lhs ^ "||" ^ conditional_to_string rhs
