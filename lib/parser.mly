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
%token LEFTARROW
%token RIGHTARROW
%token EOF
%start command

%type <Ast.redirection> redirection
%type <Ast.command> command

%%

redirection:
| LEFTARROW f = WORD { 0, f }
| opt = option(NUMBER) RIGHTARROW f = WORD { 
    match opt with
    | None -> 1, f
    | Some n -> n, f
 }

command:
| executable = WORD args = list(WORD) redirections = list(redirection) EOF { {executable; args; redirections} }
