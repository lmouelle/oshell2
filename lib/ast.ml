(** Mapping of file descriptor to file name *)
type redirection = int * string
  
type command = {
  executable : string;
  args : string list;
  redirections : redirection list;
}

let command_to_string { executable; args; redirections } =
  let redirection_to_string (fd, filename) =
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
