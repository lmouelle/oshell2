{
    open Parser
    exception SyntaxError of string
}

let whitespace = [' ' '\t']+
let string = '"' [^ '"']* '"'
let word = [^ '<' '>' ' ' '\t' '\n' '|' '&' ';' '$']+ | string
let number = ['0' - '9']+
let newline = '\n' | "\r\n"
let variable = '$' word

rule token = parse
| whitespace { token lexbuf }
| newline { NEWLINE }
| number as n '>' { RIGHTARROW(Some (int_of_string n)) }
| number as n '<' { LEFTARROW(Some (int_of_string n)) }
| '>' { RIGHTARROW (None) }
| '<' { LEFTARROW (None) }
| "&&" { AND_IF }
| "||" { OR_IF }
| '|' { PIPE }
| '&' { AMPERSAND }
| ';' { SEMICOLON }
| '=' { EQ }
| variable as lxm { VAR lxm }
| word as lxm { WORD lxm }
| eof { EOF }
| _ as lxm { raise @@ SyntaxError("Unexpected char " ^ (String.make 1 lxm)) }