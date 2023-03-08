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
(* TODO: Update the program typedef each time I add a new grammar feature *)
type program = pipeline

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

let program_to_string prog =
  List.map command_to_string prog |> String.concat "|"