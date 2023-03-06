%{
    open Ast
%}
/*
  program: command*
  command: WORD WORD* redirection*
  redirection: NUMBER? > FILENAME | < FILENAME
*/

%token <string> WORD
%token LEFTARROW
%token <int option> RIGHTARROW
%token EOF
%start command

%type <Ast.redirection> redirection
%type <Ast.command> command

%%

redirection:
| LEFTARROW f = WORD { 0, f }
| fd = RIGHTARROW f = WORD { 
  match fd with
  | None -> 1, f
  | Some n -> n, f
 }

command:
| executable = WORD args = list(WORD) redirections = list(redirection) EOF { {executable; args; redirections} }
