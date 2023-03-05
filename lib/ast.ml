type command = {
  executable : string;
  args : string list;
  (* Mapping of file descriptor to file name *)
  redirections : (int * string) list;
}

type pipeline = command list

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
