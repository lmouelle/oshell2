open OUnit2
open Oshell2.Parser
open Oshell2.Ast

let assert_cmds_equal msg parse_result expected =
  match parse_result with
  | Error msg -> assert_failure msg
  | Ok prog -> assert_equal ~msg ~printer:command_to_string prog expected

let assert_cmd_fails parse_result =
  match parse_result with
  | Ok c -> assert_failure ("Expected parse failure, got " ^ command_to_string c)
  | Error _ -> ()

let test_empty_prog _ = assert_cmd_fails (parse "")

let test_multiple_redirections _ =
  let parse_result = parse "grep foo 2> errfile > outfile" in
  assert_cmds_equal "Test multiple redirections" parse_result
    {
      executable = "grep";
      args = [ "foo" ];
      redirections = [ (2, "errfile"); (1, "outfile") ];
    }

let test_simple_prog _ =
  let parse_result = parse "cmd arg > outfile" in
  assert_cmds_equal "Test simple command redirection" parse_result
    { executable = "cmd"; args = [ "arg" ]; redirections = [ (1, "outfile") ] }

let suite =
  "Parser tests"
  >::: [
         "Test empty prog" >:: test_empty_prog;
         "Test simple prog" >:: test_simple_prog;
         "Test multi redirections" >:: test_multiple_redirections;
       ]

let _ = run_test_tt_main suite
