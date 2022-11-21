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

let holdem_tests = []

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
    quit_test "Quits using init state" state " won with an amount of 0.\n\n";
  ]
(******************************************************************
    OUnit test cases for Command
 ******************************************************************)

(*TODO*)

let command_tests = []

(******************************************************************
    OUnit test Infrastructure for Showdown
 ******************************************************************)

(** [compare_winners_lists lst1 lst2] is true if the lists contain the same
    players regardless of order *)
let compare_winners_lists (lst1 : Holdem.player list)
    (lst2 : Holdem.player list) =
  let rec compare_winners_lists_aux s1 s2 =
    match (s1, s2) with
    | [], [] -> true
    | [], _ :: _ | _ :: _, [] -> false
    | h1 :: t1, h2 :: t2 ->
        if h1.name = h2.name then compare_winners_lists_aux t1 t2 else false
  in
  let sort_winners lst =
    List.sort (fun p1 p2 -> String.compare p1.name p2.name) lst
  in
  compare_winners_lists_aux (sort_winners lst1) (sort_winners lst2)

(** [string_of_player_list players] is the string representing a list of
    transparent players *)
let rec string_of_player_list players =
  match players with
  | [ h ] -> Holdem.revealed_player_to_string h ^ "; "
  | h :: t ->
      Holdem.revealed_player_to_string h ^ "; " ^ string_of_player_list t
  | [] -> ""

(** [showdown_test name board players winners] constructs an OUnit test named
   [name] that asserts the list of player names [winners] matches the result of 
   [Showdown.showdown board players]] *)
let showdown_test (name : string) (board : Holdem.card list)
    (players : Holdem.player list) (winners : string list) : test =
  name >:: fun _ ->
  assert_equal
    (List.map
       (fun player_name ->
         {
           name = player_name;
           balance = 0;
           betting = 0;
           active = true;
           hand = [];
         })
       winners)
    (Showdown.showdown board players)
    ~cmp:compare_winners_lists ~printer:string_of_player_list

(** [cards_fold_aux acc card] is the accumulation function [f] for a
    [List.fold_left f init lst] to build a card list from suits and ranks*)
let cards_fold_aux acc (suit, rank) =
  {
    rank;
    suit =
      (match suit with
      | "H" -> Hearts
      | "S" -> Spades
      | "D" -> Diamonds
      | "C" -> Clubs
      | _ -> failwith "Invalid Suit");
  }
  :: acc

(** [make_player name cards] creates a player with [name] and [Holdem.card list]
    of [cards]*)
let make_player (name : string) (cards : (string * int) list) : Holdem.player =
  {
    name;
    balance = 0;
    betting = 0;
    active = true;
    hand = List.fold_left cards_fold_aux [] cards;
  }

(** [make_board cards] is the cards list representing a 5 card board from a list
    of suits and ranks [cards] *)
let make_board (cards : (string * int) list) : Holdem.card list =
  List.fold_left cards_fold_aux [] cards

(******************************************************************
    OUnit test cases for Showdown
 ******************************************************************)

let showdown_tests =
  [
    showdown_test "One Player"
      (make_board [ ("S", 14); ("C", 13); ("H", 10); ("D", 9); ("S", 11) ])
      [ make_player "Testy" [ ("C", 3); ("H", 7) ] ]
      [ "Testy" ];
    showdown_test "Two Players with One Winner"
      (make_board [ ("S", 14); ("C", 13); ("H", 10); ("D", 9); ("S", 11) ])
      [
        make_player "guy1" [ ("H", 14); ("H", 7) ];
        make_player "guy2" [ ("C", 13); ("H", 7) ];
      ]
      [ "guy1" ];
    showdown_test "Two Players with Two Winners"
      (make_board [ ("S", 14); ("C", 13); ("H", 10); ("D", 9); ("S", 11) ])
      [
        make_player "guy1" [ ("H", 14); ("H", 7) ];
        make_player "guy2" [ ("C", 14); ("H", 7) ];
      ]
      [ "guy1"; "guy2" ];
    showdown_test "Eight Players with One Winner"
      (make_board [ ("D", 2); ("D", 6); ("S", 2); ("S", 4); ("C", 9) ])
      [
        make_player "p1" [ ("C", 7); ("S", 12) ];
        make_player "p2" [ ("S", 6); ("D", 4) ];
        make_player "p3" [ ("C", 4); ("S", 3) ];
        make_player "p4" [ ("C", 10); ("D", 9) ];
        make_player "p5" [ ("D", 8); ("H", 9) ];
        make_player "p6" [ ("C", 2); ("S", 9) ];
        make_player "p7" [ ("C", 12); ("D", 12) ];
        make_player "p8" [ ("S", 8); ("C", 14) ];
      ]
      [ "p6" ];
  ]

(******************************************************************
    Test Suite
 ******************************************************************)

let suite =
  "test suite for 3110_final_project"
  >::: List.flatten [ holdem_tests; state_tests; command_tests; showdown_tests ]

let _ = run_test_tt_main suite