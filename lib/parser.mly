%{
    open Ast
%}
/*
  program: command*
  command: WORD WORD* redirection*
  redirection: NUMBER? > FILENAME | < FILENAME
*/

%token <int> NUMBER
%token <string> WORD
%token <string> FILENAME
%token LEFTARROW
%token RIGHTARROW
%token EOF
%start program

%type <Ast.redirection> redirection
%type <Ast.command> command
%type <Ast.program> program

%%

redirection:
| LEFTARROW f = FILENAME { 0, f }
| opt = option(NUMBER) RIGHTARROW f = FILENAME { 
    match opt with
    | None -> 1, f
    | Some n -> n, f
 }

command:
| executable = WORD args = list(WORD) redirections = list(redirection) { {executable; args; redirections} }

program:
| commands = list(command) EOF { commands }