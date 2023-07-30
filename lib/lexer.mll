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
| newline { NEWLINE }
| '>' { RIGHTARROW }
| '<' { LEFTARROW }
| "&&" { AND_IF }
| "||" { OR_IF }
| '|' { PIPE }
| '&' { AMPERSAND }
| ';' { SEMICOLON }
| word as lxm { WORD lxm }
|  '\n' { NEWLINE }
| _ as lxm { raise @@ SyntaxError("Unexpected char " ^ (String.make 1 lxm)) }