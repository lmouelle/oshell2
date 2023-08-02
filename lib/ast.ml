type redirection = {
  filename: string;
  io_num: int;
}

type assignment = {
  varval : string;
  varname : string
}

type simple_command = {
  redirections : redirection list;
  assignments : assignment list;
  args: string list;
  name: string option
}

and if_clause_test = {
  test: compound_list;
  body: compound_list
}

and compound_command = 
| CompoundCommandIf of { ifelse: compound_list option; tests : if_clause_test list }
| CompoundCommandWhile of { test: compound_list; body: compound_list}

and term = 
| TermConditional of conditional
| TermForeground of (term * conditional)
| TermBackground of (term * conditional)

and compound_list =
| CompoundListTerm of term 
| CompoundListForeground of term
| CompoundListBackground of term

and command = 
| CommandSimpleCommand of simple_command
| CommandCompoundCommand of (compound_command * redirection list)

and pipe_sequence = command list

and pipeline =
| PipelineBangSeq of pipe_sequence
| PipelineSeq of pipe_sequence

and conditional = 
| ConditionalPipeline of pipeline
| ConditionalAnd of (conditional * pipeline)
| ConditionalOr of (conditional * pipeline)

and shell_list = 
| ShellListConditional of conditional
| ShellListForeground of (shell_list * conditional)
| ShellListBackground of (shell_list * conditional)

and complete_command = 
| CompleteCommandShellList of shell_list
| CompleteCommandForeground of shell_list
| CompleteCommandBackground of shell_list

and program = complete_command list option