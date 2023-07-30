%{
    open Ast
%}

%token <string> WORD
%token <string> ASSIGNMENT_WORD
%token NEWLINE

%token SEMICOLON ";"
%token AMPERSAND "&"
%token <int option> RIGHTARROW ">"
%token <int option> LEFTARROW "<"
%token PIPE "|"

%token EOF

/* The following are the operators (see XBD Operator)
   containing more than one character. */
/* && || */
%token AND_IF OR_IF

/* The following are the reserved words. */
%token If Then Else Elif Fi Do Done
%token While 

/* These are reserved words, not operator tokens, and are
   recognized when reserved words are recognized. */

%token Bang

%start program

%type <Ast.program> program
%type <Ast.complete_command> complete_command
%type <Ast.shell_list> shell_list
%type <Ast.conditional> and_or
%type <Ast.pipeline> pipeline
%type <Ast.command> command
%type <Ast.compound_list> compound_list
%type <Ast.term> term
%type <Ast.compound_command> compound_command
%type <Ast.simple_command> simple_command
%type <Ast.redirection> io_redirect

%%

program:
| linebreak cmds = complete_commands linebreak EOF { Some cmds }
| linebreak EOF { None }

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
| cond = and_or { ShellListConditional cond }

and_or:
| p = pipeline { ConditionalPipeline p }
| cond = and_or AND_IF linebreak p = pipeline { ConditionalAnd(cond, p)  }
| cond = and_or OR_IF linebreak p = pipeline { ConditionalOr(cond, p) }

pipeline:
| p = pipe_sequence { PipelineSeq p }
| Bang p = pipe_sequence { PipelineBangSeq p }

pipe_sequence: 
| c = command { [c] }
| seq = pipe_sequence "|" linebreak c = command { seq @ [c] }

command:
| c = simple_command { CommandSimpleCommand c }
| c = compound_command { CommandCompoundCommand(c, []) }
| c = compound_command newredirs = redirect_list 
{ 
  CommandCompoundCommand(c, newredirs)
}

simple_command:
| prefix = cmd_prefix cmd = cmd_word suffix = cmd_suffix {
  let redirections_prefix = List.filter_map (fun (_, b) -> b) prefix in
  let assignments = List.filter_map (fun (a, _) -> a) prefix in
  let redirections_suffix = List.filter_map (fun (_, b) -> b) suffix in
  let args = List.filter_map (fun (a, _) -> a) suffix in
  let redirections = redirections_prefix @ redirections_suffix in
  {name = Some cmd; redirections; assignments; args}
}
| prefix = cmd_prefix cmd = cmd_word {
  let redirections = List.filter_map (fun (_, b) -> b) prefix in
  let assignments = List.filter_map (fun (a, _) -> a) prefix in
  {name = Some cmd; redirections; assignments; args = []}
}
| prefix = cmd_prefix { 
  let redirections = List.filter_map (fun (_, b) -> b) prefix in
  let assignments = List.filter_map (fun (a, _) -> a) prefix in
  {name = None; redirections; assignments; args = []}
}
| cmd = cmd_name suffix = cmd_suffix {  
  let redirections = List.filter_map (fun (_, b) -> b) suffix in
  let args = List.filter_map (fun (a, _) -> a) suffix in
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

cmd_word:
| w = WORD { w }

compound_command:
| cmd = if_clause { cmd }
| cmd = while_clause { cmd }

if_clause:
| If test = compound_list Then body = compound_list rem = else_part Fi {
  let (elsepart, lst) = rem in
  let tests = {test; body} :: lst in
  CompoundCommandIf {ifelse = elsepart; tests}
}
| If test = compound_list Then body = compound_list Fi { 
  let tests = [{test; body}] in
  CompoundCommandIf {ifelse = None; tests}
}

/*type is (a * b), where a is 'compount_list option' representing the else clause, and
 b is a list of pairs of tests and bodies.
 
 example:

 if test0 then body0 "elif test1 then body1 else body2" // first few words are matched by if_clause
 produces:

 */
else_part:
| Elif test = compound_list Then body = compound_list { 
  (None, [{test; body}])
 }
| Elif test = compound_list Then body = compound_list rem = else_part {
  let (elsepart, lst) = rem in
  (elsepart, {test; body} :: lst)
}
| Else remain = compound_list { (Some remain, []) }

redirect_list:
| redir = io_redirect { [redir] }
| lst = redirect_list redir = io_redirect { lst @ [redir] }

io_redirect:
| io_num_opt = RIGHTARROW file = filename {
  let io_num = match io_num_opt with
  | None -> 1
  | Some n -> n
  in
  {filename = file; io_num}
}
| io_num_opt = LEFTARROW file = filename {
  let io_num = match io_num_opt with
  | None -> 0
  | Some n -> n
  in
  {filename = file; io_num}
}

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
| newline_list { 0 }
| /* empty */ { 0 }

newline_list:
| NEWLINE { 0 }
| newline_list NEWLINE { 0 }

while_clause: 
| While test = compound_list body = do_group {
  CompoundCommandWhile {test; body}
}

do_group: 
| Do lst = compound_list Done {
  lst
}
