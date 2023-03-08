type redirection_type = Input | Output

type redirection = {
  redirection_type : redirection_type;
  filename : string;
  file_desc : int;
}
  
type command = {
  executable : string;
  args : string list;
  redirections : redirection list;
}

type pipeline = command list

type conditional = 
| BasePipeline of pipeline
| And of conditional * conditional
| Or of conditional * conditional

(* TODO: Update the program typedef each time I add a new grammar feature.
   Requires updating eveything everywhere else. So tedious. Perhaps investigate using modules
   for encapsulation? *)
type program = conditional

let command_to_string { executable; args; redirections } =
  let redirection_to_string {filename; file_desc = fd; _} =
    string_of_int fd ^ "->" ^ filename
  in
  let bare_string =
    [
      executable;
      String.concat " " args;
      List.map redirection_to_string redirections |> String.concat ",";
    ]
    |> String.concat ";"
  in
  "{" ^ bare_string ^ "}"

let pipeline_to_string pipeline =
  List.map command_to_string pipeline |> String.concat "|"

let rec conditional_to_string = function
| BasePipeline p -> pipeline_to_string p
| And(lhs, rhs) -> 
  conditional_to_string lhs ^ "&&" ^ conditional_to_string rhs
| Or(lhs, rhs) ->  
  conditional_to_string lhs ^ "||" ^ conditional_to_string rhs
