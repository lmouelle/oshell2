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

%token <string> WORD
%token <string> ASSIGNMENT_WORD
%token <string> NAME
%token NEWLINE
%token <int> IO_NUMBER

%token AND_IF OR_IF
%token DLESS DGREAT

%token If Then Else Elif Fi Do Done
%token While 

%token Lbrace Rbrace Bang
%token In

%start program

%type <Ast.program> program

%%

program:
| linebreak cmds = complete_commands linebreak  { Some cmds }
| linebreak { None }

complete_commands: 
| lst = complete_commands newline_list item = complete_command { lst @ [item] }
| item = complete_command { [item] }

complete_command:
| lst = shell_list ";" { CompleteCommandForeground lst }
| lst = shell_list "&" { CompleteCommandBackground lst }
| lst = shell_list { CompleteCommandShellList lst }

shell_list:
| lst = shell_list ";" cond = and_or { ShellListForeground (lst, cond) }
| lst = shell_list "&" cond = and_or { ShellListBackground (lst, cond) }
| cond = and_or { ShellListCondtional cond }

and_or:
| p = pipeline { ConditionalPipeline p }
| cond = and_or AND_IF linebreak p = pipeline { ConditionalAnd(cond, p)  }
| cond = and_or OR_IF linebreak p = pipeline { ConditionalOr(cond, p) }

pipeline:
| p = pipe_sequence { p }

pipe_sequence: 
| c = command { [c] }
| seq = pipe_sequence "|" linebreak c = command { seq @ [c] }

command:
| c = simple_command { CommandSimpleCommand c }
| c = compound_command { CommandCompoundCommand c }
| c = compound_command newredirs = redirect_list 
{ 
  let oldredirs = c.redirs in
  let c' = {c with redirs = (oldredirs @ newredirs)} in
  CommandCompoundCommand(c')
}

compound_command:
| if_clause

if_clause:
| If test = compound_list Then body = compound_list rem = else_part Fi {
  let (elsepart, lst) = rem in
  CompoundCommandIf {ifelse = elsepart, tests = {test, body} :: lst}
}
| If test = compound_list Then body = compound_list Fi { CompoundCommandIf {ifelse = None, tests = [{test, body}]} }

/*type is (a * b), where a is 'compount_list option' representing the else clause, and
 b is a list of pairs of tests and bodies.
 
 example:

 if test0 then body0 "elif test1 then body1 else body2" // first few words are matched by if_clause
 produces:

 */
else_part:
| Elif test = compound_list Then body = compound_list { 
  (None, {test, body})
 }
| Elif test = compound_list Then body = compound_list rem = else_part {
  let (elsepart, lst) = rem in
  (elsepart, {test, body} :: lst)
}
| Else remain = compound_list { (Some remain, []) }

redirect_list:
| io_redirect
| redirect_list io_redirect

io_redirect:
| io_file
| IO_NUMBER io_file

io_file:
| "<" filename
| DLESS filename
| DGREAT filename

filename:
| WORD

term:
| term seperator and_or
| and_or

compound_list:
| linebreak term
| linebreak term seperator

linebreak: 
| newline_list 
| /* empty */

newline_list:
| NEWLINE
| newline_list NEWLINE

seperator:
| seperator_op linebreak
| newline_list
