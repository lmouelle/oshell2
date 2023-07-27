{
    open Parser
    exception SyntaxError of string
}

let whitespace = [' ' '\t']+
let string = '"' [^ '"']* '"'
let word = [^ '<' '>' ' ' '\t']+ | string
let number = ['0' - '9']+
let newline = '\n' | "\r\n"
let variable = '$' word

rule token = parse
| whitespace { token lexbuf }
| newline { NEWLINE }
| '>' { RIGHTARROW }
| '<' { LEFTARROW }
| "&&" { AND }
| "||" { OR }
| '|' { PIPE }
| '=' { EQ }
| '&' { AMPERSAND }
| ';' { SEMICOLON }
| variable as lxm { VAR lxm }
| word as lxm { WORD lxm }
| eof { EOF } 
| _ as lxm { raise @@ SyntaxError("Unexpected char " ^ (String.make 1 lxm)) }