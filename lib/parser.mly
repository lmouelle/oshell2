%{
    open Ast
%}
/*
  program: pipeline EOF
  pipeline: command (| command)*
  command: WORD WORD* redirection*
  
  // Note there is no spaces permitted between FILE_DESC and >
  redirection: FILE_DESC? > FILENAME | FILE_DESC? < FILENAME 
*/

%token <string> WORD
%token <int option> LEFTARROW
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
| fd = LEFTARROW f = WORD { 
  match fd with
  | None -> { redirection_type = Input; filename = f; file_desc = 0 }
  | Some n -> { redirection_type = Input; filename = f; file_desc = n }
}
| fd = RIGHTARROW f = WORD { 
  match fd with
  | None -> { redirection_type = Input; filename = f; file_desc = 1 }
  | Some n -> { redirection_type = Input; filename = f; file_desc = n }
 }

command:
| executable = WORD args = list(WORD) redirections = list(redirection) { {executable; args; redirections} }

pipeline:
| commands = separated_nonempty_list(PIPE, command) { commands }

program:
| pipeline EOF { $1 }
