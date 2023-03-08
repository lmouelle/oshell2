{
    open Parser
    exception SyntaxError of string
}

let whitespace = [' ' '\t']+
let string = '"' [^ '"']* '"'
let word = [^ '<' '>' ' ' '\t']+ | string
let number = ['0' - '9']+
let newline = '\n' | "\r\n"

rule token = parse
| whitespace { token lexbuf }
| newline { token lexbuf }
| number as n '>' { RIGHTARROW(Some (int_of_string n)) }
| '>' {RIGHTARROW(None)}
| '<' { LEFTARROW }
| '|' { PIPE }
| word as lxm { WORD lxm }
| eof { EOF } 
| _ as lxm { raise @@ SyntaxError("Unexpected char " ^ (String.make 1 lxm)) }