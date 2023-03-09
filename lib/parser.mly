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
%token AND OR
%token EOF
%start program

%type <Ast.redirection> redirection
%type <Ast.command> command
%type <Ast.pipeline> pipeline
%type <Ast.conditional> conditional
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
  | None -> { redirection_type = Output; filename = f; file_desc = 1 }
  | Some n -> { redirection_type = Output; filename = f; file_desc = n }
 }

command:
| executable = WORD args = list(WORD) redirections = list(redirection) { {executable; args; redirections} }

pipeline:
| commands = separated_list(PIPE, command) { commands }

conditional:
| p = pipeline { BasePipeline p }
| p1 = pipeline AND p2 = pipeline { And(BasePipeline(p1), BasePipeline(p2)) }
| p1 = pipeline OR p2 = pipeline { Or(BasePipeline(p1), BasePipeline(p2)) }

program:
| c = conditional EOF { c }
