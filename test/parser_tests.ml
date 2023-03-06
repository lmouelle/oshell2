open OUnit2
open Oshell2.Parseutils
open Oshell2.Ast

let assert_command_equals = assert_equal ~printer:command_to_string

let test_multi_redirect _ =
  let command = parse_string "ls -l 2> errfile > outfile" in
  assert_command_equals ~msg:"Test mutli redirects" command
    {
      executable = "ls";
      args = [ "-l" ];
      redirections = [ (2, "errfile"); (1, "outfile") ];
    }

let suite =
  "Parser tests" >::: [ "Test multi redirect" >:: test_multi_redirect ]

let _ = run_test_tt_main suite
