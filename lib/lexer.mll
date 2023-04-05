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
| newline { token lexbuf }
| number as n '>' { RIGHTARROW(Some (int_of_string n)) }
| number as n '<' { LEFTARROW(Some (int_of_string n)) }
| '>' { RIGHTARROW(None) }
| '<' { LEFTARROW(None) }
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