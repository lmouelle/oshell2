{
    open Parser
    exception SyntaxError of string
}

let whitespace = [' ' '\t']+
let word = ['a'-'z' 'A'-'Z' '0'-'9' '_' '-']+
let filename = [^ '\x00']+
let number = ['0' - '9']+
let newline = '\n' | "\r\n"

rule token = parse
| whitespace { token lexbuf }
| newline { token lexbuf }
| '>' { RIGHTARROW }
| '<' { LEFTARROW }
| number as lxm { NUMBER(int_of_string lxm) }
| word as lxm { WORD lxm }
| filename as lxm { FILENAME lxm }
| eof { EOF } 
| _ as lxm { raise @@ SyntaxError("Unexpected char " ^ (String.make 1 lxm)) }