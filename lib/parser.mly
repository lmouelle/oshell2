%{
    open Ast
%}
/*
  program: conditional EOF
  conditional: pipeline (cond_op conditional)*
  cond_op: || | &&
  pipeline: command (| command)*
  command: assignment* WORD WORD* redirection* | assignment+
  
  // Note there is no spaces permitted between FILE_DESC and >
  redirection: FILE_DESC? > FILENAME | FILE_DESC? < FILENAME 
*/

%token <string> VAR
%token <string> WORD
%token <int option> LEFTARROW
%token <int option> RIGHTARROW
%token PIPE
%token EQ
%token AND OR
%token EOF
%start program assignment

%type <Ast.redirection> redirection
%type <Ast.command> command
%type <Ast.pipeline> pipeline
%type <Ast.conditional> conditional
%type <Ast.program> program
%type <Ast.variable_entry> assignment

%%

assignment:
| varname = VAR EQ varval = WORD { (varname, String varval) }
| varname = VAR EQ varval = VAR { (varname, Variable varval) }

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
| variables = list(assignment) executable = WORD args = list(WORD) redirections = list(redirection) 
{ {executable; args; redirections; variables} }
| variables = assignment+
{ {executable = String.empty; args = []; redirections = []; variables} }

pipeline:
| commands = separated_list(PIPE, command) { commands }

conditional:
| p = pipeline { BasePipeline p }
| p1 = pipeline AND p2 = conditional { And(BasePipeline(p1), p2) }
| p1 = pipeline OR p2 = conditional { Or(BasePipeline(p1), p2) }

program:
| c = conditional EOF { c }
