type command = { 
  executable : string; 
  args: string list;  
  (* Mapping of file descriptor to file name *)
  redirections : (int * string) list
}

type pipeline = command list

