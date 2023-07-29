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

/* && || */
%token AND_IF OR_IF

%token If Then Else Elif Fi Do Done
%token While 

%token Lbrace Rbrace Bang
%token In

%token EOL

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

simple_command:
| prefix = cmd_prefix cmd = cmd_word suffix = cmd_suffix {
  let redirections_prefix = List.map (fun (a, b) -> b) prefix in
  let assignments = List.map (fun (a, b) -> a) prefix in
  let redirections_suffix = List.map (fun (a, b) -> b) suffix in
  let args = List.map (fun (a, b) -> a) suffix in
  let redirections = redirections_prefix @ redirections_suffix in
  {name = Some cmd; redirections; assignments; args}
}
| prefix = cmd_prefix cmd = cmd_word {
  let redirections = List.map (fun (a, b) -> b) prefix in
  let assignments = List.map (fun (a, b) -> a) prefix in
  {name = Some cmd; redirections; assignments; args = []}
}
| prefix = cmd_prefix { 
  let redirections = List.map (fun (a, b) -> b) prefix in
  let assignments = List.map (fun (a, b) -> a) prefix in
  {name = None; redirections; assignments; args = []}
}
| cmd = cmd_name suffix = cmd_suffix {  
  let redirections = List.map (fun (a, b) -> b) suffix in
  let args = List.map (fun (a, b) -> a) suffix in
  {name = Some cmd; redirections; assignments = []; args}
}
| cmd = cmd_name { {name = Some cmd; redirections = []; assignments = []; args = []} }

cmd_prefix:
| iof = io_redirect { [None, Some iof] }
| rest = cmd_prefix iof = io_redirect { 
  rest @ [None, Some iof]
 }
| w = ASSIGNMENT_WORD { [Some w, None] }
| rest = cmd_prefix w = ASSIGNMENT_WORD { 
  rest @ [Some w, None]
}

cmd_suffix:
| iof = io_redirect { [None, Some iof] }
| rest = cmd_suffix iof = io_redirect  { 
  rest @ [None, Some iof]
}
| w = WORD { [Some w, None] }
| rest = cmd_suffix w = WORD { 
  rest @ [Some w, None]
}

cmd_name: 
| w = WORD { w }

compound_command:
| cmd = if_clause { cmd }

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
| redir = io_redirect { [redir] }
| lst = redirect_list redir = io_redirect { lst @ [redir] }

io_redirect:
| iof = io_file { iof }
| num = IO_NUMBER f = io_file {
  {f with io_num = num}
 }

io_file:
| ">" f = filename { {filename = f, io_num = 0} }
| "<" f = filename { {filename = f, io_num = 1} }

filename:
| w = WORD { w }

term:
| t = term "&" linebreak cond = and_or {
  TermBackground (t, cond)
}
| t = term ";" linebreak cond = and_or {
  TermForeground (t, cond)
}
| cond = and_or {
  TermConditional cond
}

compound_list:
| linebreak t = term { 
  CompoundListTerm t
 }
| linebreak t = term "&" linebreak {
  CompoundListBackground t
}
| linebreak t = term ";" linebreak {
  CompoundListForeground t
}

linebreak:
| newline_list { None }
| /* empty */ { None }

newline_list:
| NEWLINE { None }
| newline_list NEWLINE { None }

