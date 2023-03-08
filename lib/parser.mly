%{
    open Ast
%}
/*
  program: pipeline EOF
  pipeline: command (| command)*
  command: WORD WORD* redirection*
  redirection: NUMBER? > FILENAME | < FILENAME
*/

%token <string> WORD
%token LEFTARROW
%token <int option> RIGHTARROW
%token PIPE
%token EOF
%start program

%type <Ast.redirection> redirection
%type <Ast.command> command
%type <Ast.pipeline> pipeline
%type <Ast.program> program

%%

redirection:
| LEFTARROW f = WORD { 0, f }
| fd = RIGHTARROW f = WORD { 
  match fd with
  | None -> 1, f
  | Some n -> n, f
 }

command:
| executable = WORD args = list(WORD) redirections = list(redirection) { {executable; args; redirections} }

pipeline:
| commands = separated_nonempty_list(PIPE, command) { commands }

program:
| pipeline EOF { $1 }
