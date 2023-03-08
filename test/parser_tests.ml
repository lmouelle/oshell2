open OUnit2
open Oshell2.Parseutils
open Oshell2.Ast

let assert_command_equals = assert_equal ~printer:program_to_string

let test_multi_redirect _ =
  let command = parse_string "ls -l 2> errfile > outfile" in
  assert_command_equals ~msg:"Test mutli redirects" command
    [
      {
        executable = "ls";
        args = [ "-l" ];
        redirections =
          [
            { file_desc = 2; filename = "errfile"; redirection_type = Output };
            { file_desc = 1; filename = "outfile"; redirection_type = Output };
          ];
      };
    ]

let test_simple_command _ =
  let command = parse_string "ls -l -a -Z" in
  assert_command_equals ~msg:"Test simple command" command
    [ { executable = "ls"; args = [ "-l"; "-a"; "-Z" ]; redirections = [] } ]

let test_redir_in_and_out _ =
  let command =
    parse_string "foo -Z < infile 2> errfile > outfile 666> etcfile"
  in
  assert_command_equals ~msg:"Test redir in and out" command
    [
      {
        executable = "foo";
        args = [ "-Z" ];
        redirections =
          [
            { file_desc = 0; redirection_type = Input; filename = "infile" };
            { file_desc = 2; filename = "errfile"; redirection_type = Output };
            { file_desc = 1; filename = "outfile"; redirection_type = Output };
            { file_desc = 666; filename = "etcfile"; redirection_type = Output };
          ];
      };
    ]

let suite =
  "Parser tests"
  >::: [
         "Test multi redirect" >:: test_multi_redirect;
         "Test simple command" >:: test_simple_command;
         "Test redire in and out" >:: test_redir_in_and_out;
       ]

let _ = run_test_tt_main suite
