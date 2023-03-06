open Ast
open Angstrom

(*program: command*
  command: WORD WORD* redirection*
  redirection: NUMBER? > FILENAME | < FILENAME*)

let is_seperating_whitespace = function ' ' | '\t' -> true | _ -> false

let is_word_char = function
  | ' ' | '\t' | '\r' | '\n' | '|' | '$' | '&' | ';' | '>' | '<' -> false
  | _ -> true

let whitespace_dropping_parser = skip_while is_seperating_whitespace

let word_parser = take_while1 is_word_char

let filename_parser = take_while1 (fun c -> not @@ is_seperating_whitespace c && c <> '\x00')

let number_parser =
  take_while1 (function '0' .. '9' -> true | _ -> false)
  >>| int_of_string 


let redirection_parser =
  option None (number_parser >>= fun n -> return @@ Some n) >>= fun numopt ->
  char '<' <|> char '>' >>= fun arrow ->
  whitespace_dropping_parser *>
  filename_parser >>= fun filename ->
  match (numopt, arrow) with
  | None, '<' -> return @@ (0, filename)
  | Some n, '<' ->
      fail ("Unexpected number " ^ string_of_int n ^ " in stdin redirect")
  | None, '>' -> return @@ (1, filename)
  | Some n, '>' -> return @@ (n, filename)
  | None, c | Some _, c -> fail ("Unexpected redirect char " ^ String.make 1 c)

let command_parser =
  whitespace_dropping_parser *>
  word_parser >>= fun executable ->
  whitespace_dropping_parser *> 
  sep_by whitespace_dropping_parser word_parser >>= fun args ->
  sep_by whitespace_dropping_parser redirection_parser >>= fun redirections ->
  whitespace_dropping_parser *>
  return { executable; args; redirections }

let parse_with parser str = parse_string ~consume:All parser str
  
let parse = parse_with command_parser
