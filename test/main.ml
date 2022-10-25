open OUnit2
open Game
open Holdem
open State
open Command

(******************************************************************
    OUnit test cases for Holdem
 ******************************************************************)

(**[top_card_test name deck expected_output] constructs an OUnit test named
   [name] that asserts the quality of [expected_output] with [top_card deck].*)
let top_card_test (name : string) deck expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (top_card deck |> card_to_string)

(**[shuffle_deck_test shuffled_deck fresh_deck expected_result] constructs an
   OUnit test named [name] that asserts the quality of [expected_result] with
   [deck_to_string shuffled_deck <> deck_to_string fresh_deck].*)
let shuffle_deck_test (name : string) shuffled_deck fresh_deck expected_result :
    test =
  name >:: fun _ ->
  assert_equal expected_result
    (cards_to_string shuffled_deck <> cards_to_string fresh_deck)

let holdem_tests =
  [
    top_card_test "Top card of a fresh deck is the Ace of Spades" fresh_deck
      "AS";
  ]

(******************************************************************
    OUnit test cases for State
 ******************************************************************)

(**[quit_test name st expected_output] constructs an OUnit test named [name]
   that asserts the quality of [expected_output] with [state_to_string st].*)
let quit_test (name : string) (st : State.t) (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (quit st)

(**[state_to_string_test name st expected_output] constructs an OUnit test named
   [name] that asserts the quality of [expected_output] with
   [state_to_string st].*)
let state_to_string_test (name : string) (st : State.t)
    (expected_output : string) : test =
  (*let () = print_string ("\n" ^ state_to_string st ^ "\n") in*)
  name >:: fun _ -> assert_equal expected_output (state_to_string st)

let state_tests =
  let state = init 50 in
  [
    state_to_string_test "State to string using init state" state
      "TABLE:\n\
       No current players\n\
       Pot: 0 Chips\n\
       Board: __ __ __ __ __\n\n\
       You can add/remove players or deal cards";
    quit_test "Quits using init state" state " won with an amount of 0\n\n";
  ]
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
  >::: List.flatten [ holdem_tests; state_tests; command_tests ]

let _ = run_test_tt_main suite