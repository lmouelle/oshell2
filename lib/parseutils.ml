let parse_string program_string =
  let lexbuf = Lexing.from_string program_string in
  Parser.command Lexer.token lexbuf

let parse_file filename =
  let channel = open_in filename in
  let lexbuf = Lexing.from_channel channel in
  let prog = Parser.command Lexer.token lexbuf in
  close_in channel;
  prog
