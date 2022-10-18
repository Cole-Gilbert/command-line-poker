open OUnit2
open Game
open Holdem
open Command

(******************************************************************
    OUnit test cases for Holdem
 ******************************************************************)

(**[top_card name deck expected_output] constructs an OUnit test named [name]
   that asserts the quality of [expected_output] with [top_card deck].*)
let top_card (name : string) deck expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (top_card deck |> card_to_string)

let holdem_tests =
  [ top_card "Top card of a fresh deck is the Ace of Spades" fresh_deck "AS" ]

(******************************************************************
    OUnit test cases for Command
 ******************************************************************)

(*TODO*)

let command_tests = []

(******************************************************************
    Test Suite
 ******************************************************************)

let suite =
  "test suite for 3110_final_project"
  >::: List.flatten [ holdem_tests; command_tests ]

let _ = run_test_tt_main suite