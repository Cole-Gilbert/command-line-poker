open OUnit2
open Game
open Holdem
open State
open Command

(* -----------------------------------------------------------------------------
   Testing Approach: To test this project we employed a variety of techniques
   including black box, randomized testing, and glass box testing within OUnit,
   as well as manual testing via the "make play" command. Most of the functions
   within holdem.ml were tested intrinsically with our test cases for other
   modules since most of the other modules relied on the types and functions
   defined there. The specific functions we tested within holdem.ml (like
   shuffled deck) are specified below. Our showdown.ml tests began as glass box
   tests, targeting specific scenarios that would reach edge cases within the
   various showdown.ml functions. For example, we often tested hands that were
   nearly equal in rank to ensure the "kickers" aspect of each hand was
   accurately evaluated for correctness. We added many more randomized tests to
   ensure that showdown.ml was accurately calculating the winner. Our command.ml
   and state.ml tests were tested using black box testing. Based on the given
   command, we would test to make sure the state changed accordingly. We also
   tested state.ml and command.ml manually by using "make play" and playing
   games amongst ourselves and ensuring the gameplay progressed correctly. As a
   result of this manual testing, the test cases for state.ml and command.ml
   aren't completely extensive. These various test cases prove the correctness
   of our program because they use a variety of testing techniques (including
   black box, glass box, and randomized) and they have discovered errors and
   aided in the correction of those issues. In addition to these testing
   techniques we played countless games using "make play" throughout the
   development of our poker game and therefore have continued to prove the
   correctness of our project as a whole.
   ---------------------------------------------------------------------------*)

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
    (cards_to_string shuffled_deck = cards_to_string fresh_deck)

let holdem_tests =
  [
    shuffle_deck_test "Ensure decks are being randomized"
      (Holdem.shuffled_deck ()) (Holdem.shuffled_deck ()) true;
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
  name >:: fun _ ->
  assert_equal expected_output (state_to_string st) ~printer:(fun s -> s)

let init_state_str =
  {|TABLE:
No current players
Pot: 0 Chips
Board: (Min Bet: 0)
 ┌─────────┐  ┌─────────┐  ┌─────────┐  ┌─────────┐  ┌─────────┐ 
 │░░░░░░░░░│  │░░░░░░░░░│  │░░░░░░░░░│  │░░░░░░░░░│  │░░░░░░░░░│ 
 │░░░░░░░░░│  │░░░░░░░░░│  │░░░░░░░░░│  │░░░░░░░░░│  │░░░░░░░░░│ 
 │░░░░░░░░░│  │░░░░░░░░░│  │░░░░░░░░░│  │░░░░░░░░░│  │░░░░░░░░░│ 
 │░░░░░░░░░│  │░░░░░░░░░│  │░░░░░░░░░│  │░░░░░░░░░│  │░░░░░░░░░│ 
 │░░░░░░░░░│  │░░░░░░░░░│  │░░░░░░░░░│  │░░░░░░░░░│  │░░░░░░░░░│ 
 │░░░░░░░░░│  │░░░░░░░░░│  │░░░░░░░░░│  │░░░░░░░░░│  │░░░░░░░░░│ 
 │░░░░░░░░░│  │░░░░░░░░░│  │░░░░░░░░░│  │░░░░░░░░░│  │░░░░░░░░░│ 
 └─────────┘  └─────────┘  └─────────┘  └─────────┘  └─────────┘ 

You can add/remove players or deal cards|}

let state_tests =
  let init_state = init 50 in
  [
    state_to_string_test "Init state test" init_state init_state_str
    (*state_to_string_test "State to string using init state" state
      state_test_str; quit_test "Quits using init state" state " won with an
      amount of 0.\n\n"; *);
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
    showdown_test "Six Players with Two Winners"
      (make_board [ ("C", 14); ("D", 12); ("C", 12); ("S", 9); ("S", 4) ])
      [
        make_player "p1" [ ("H", 9); ("D", 2) ];
        make_player "p2" [ ("D", 13); ("D", 9) ];
        make_player "p3" [ ("D", 6); ("D", 8) ];
        make_player "p4" [ ("S", 13); ("D", 11) ];
        make_player "p5" [ ("C", 2); ("H", 3) ];
        make_player "p6" [ ("H", 2); ("H", 5) ];
      ]
      [ "p1"; "p2" ];
    showdown_test "Two Players with One Winner (Three of a Kind)"
      (make_board [ ("D", 12); ("D", 5); ("C", 12); ("H", 4); ("S", 2) ])
      [
        make_player "p1" [ ("H", 6); ("S", 12) ];
        make_player "p2" [ ("H", 9); ("C", 5) ];
      ]
      [ "p1" ];
    showdown_test "Two Players with One Winner (Four of a Kind)"
      (make_board [ ("D", 12); ("D", 5); ("C", 12); ("H", 4); ("S", 2) ])
      [
        make_player "p1" [ ("H", 12); ("S", 12) ];
        make_player "p2" [ ("H", 9); ("C", 5) ];
      ]
      [ "p1" ];
    showdown_test "High Cards"
      (make_board [ ("D", 12); ("D", 5); ("C", 11); ("H", 4); ("S", 2) ])
      [
        make_player "p1" [ ("H", 14); ("S", 8) ];
        make_player "p2" [ ("H", 13); ("C", 3) ];
      ]
      [ "p1" ];
    showdown_test "High Cards chop"
      (make_board [ ("D", 12); ("D", 5); ("C", 11); ("H", 4); ("S", 2) ])
      [
        make_player "p1" [ ("D", 13); ("S", 3) ];
        make_player "p2" [ ("H", 13); ("C", 3) ];
      ]
      [ "p1"; "p2" ];
    showdown_test "High Cards last kicker"
      (make_board [ ("D", 12); ("D", 5); ("C", 11); ("H", 4); ("S", 2) ])
      [
        make_player "p1" [ ("H", 14); ("S", 8) ];
        make_player "p2" [ ("D", 14); ("C", 7) ];
      ]
      [ "p1" ];
    showdown_test "High Cards"
      (make_board [ ("D", 12); ("D", 5); ("C", 11); ("H", 4); ("S", 2) ])
      [
        make_player "p1" [ ("H", 14); ("S", 3) ];
        make_player "p2" [ ("H", 13); ("C", 3) ];
      ]
      [ "p1" ];
    showdown_test "High Card vs Pair"
      (make_board [ ("D", 12); ("D", 6); ("H", 7); ("C", 4); ("S", 2) ])
      [
        make_player "p1" [ ("H", 14); ("S", 3) ];
        make_player "p2" [ ("H", 12); ("C", 3) ];
      ]
      [ "p2" ];
    showdown_test "Pair vs Pair"
      (make_board [ ("D", 12); ("D", 6); ("H", 7); ("C", 4); ("S", 2) ])
      [
        make_player "p1" [ ("D", 7); ("S", 3) ];
        make_player "p2" [ ("H", 12); ("C", 3) ];
      ]
      [ "p2" ];
    showdown_test "Pair vs Pair Kicker Tied"
      (make_board [ ("D", 12); ("D", 6); ("H", 7); ("C", 4); ("S", 2) ])
      [
        make_player "p1" [ ("S", 12); ("S", 3) ];
        make_player "p2" [ ("H", 12); ("C", 3) ];
      ]
      [ "p1"; "p2" ];
    showdown_test "Pair vs Pair Kicker 1"
      (make_board [ ("D", 12); ("D", 6); ("H", 7); ("C", 4); ("S", 2) ])
      [
        make_player "p1" [ ("S", 12); ("S", 13) ];
        make_player "p2" [ ("H", 12); ("C", 3) ];
      ]
      [ "p1" ];
    showdown_test "Pair vs Pair Kicker 2"
      (make_board [ ("H", 13); ("D", 6); ("H", 11); ("C", 4); ("S", 2) ])
      [
        make_player "p1" [ ("C", 13); ("S", 9) ];
        make_player "p2" [ ("D", 13); ("C", 10) ];
      ]
      [ "p2" ];
    showdown_test "Pair vs Pair Kicker 3"
      (make_board [ ("H", 13); ("D", 6); ("H", 11); ("C", 14); ("S", 2) ])
      [
        make_player "p1" [ ("C", 13); ("S", 9) ];
        make_player "p2" [ ("D", 13); ("C", 10) ];
      ]
      [ "p2" ];
    showdown_test "Pair vs Pair Kicker 4 not in play"
      (make_board [ ("H", 13); ("D", 6); ("H", 11); ("C", 14); ("S", 10) ])
      [
        make_player "p1" [ ("C", 13); ("S", 8) ];
        make_player "p2" [ ("D", 13); ("C", 7) ];
      ]
      [ "p1"; "p2" ];
    showdown_test "Pair vs Pair Kicker 4 not in play"
      (make_board [ ("H", 13); ("D", 6); ("H", 11); ("C", 14); ("S", 10) ])
      [
        make_player "p1" [ ("C", 13); ("S", 8) ];
        make_player "p2" [ ("D", 13); ("C", 7) ];
      ]
      [ "p1"; "p2" ];
    showdown_test "Pair vs Two Pair"
      (make_board [ ("C", 7); ("H", 3); ("H", 6); ("D", 2); ("H", 12) ])
      [
        make_player "p1" [ ("C", 3); ("S", 6) ];
        make_player "p2" [ ("D", 7); ("C", 14) ];
      ]
      [ "p1" ];
    showdown_test "Two Pair vs Two Pair Top Pair Wins"
      (make_board [ ("C", 7); ("H", 3); ("H", 6); ("D", 2); ("H", 12) ])
      [
        make_player "p1" [ ("C", 3); ("S", 6) ];
        make_player "p2" [ ("D", 7); ("C", 2) ];
      ]
      [ "p2" ];
    showdown_test "Two Pair vs Two Pair Lower Pair"
      (make_board [ ("C", 7); ("H", 3); ("H", 6); ("D", 2); ("H", 12) ])
      [
        make_player "p1" [ ("C", 3); ("S", 7) ];
        make_player "p2" [ ("D", 7); ("C", 2) ];
      ]
      [ "p1" ];
    showdown_test "Two Pair vs Two Pair Kicker"
      (make_board [ ("C", 2); ("H", 3); ("H", 6); ("D", 2); ("H", 12) ])
      [
        make_player "p1" [ ("S", 3); ("H", 12) ];
        make_player "p2" [ ("D", 7); ("D", 3) ];
      ]
      [ "p1" ];
    showdown_test "Two Pair vs Three of a Kind"
      (make_board [ ("C", 2); ("H", 3); ("H", 6); ("D", 2); ("H", 12) ])
      [
        make_player "p1" [ ("S", 3); ("H", 12) ];
        make_player "p2" [ ("C", 3); ("D", 3) ];
      ]
      [ "p2" ];
    showdown_test "Three of a Kind vs Three of a Kind"
      (make_board [ ("C", 2); ("H", 3); ("H", 6); ("D", 2); ("H", 12) ])
      [
        make_player "p1" [ ("S", 2); ("H", 12) ];
        make_player "p2" [ ("C", 3); ("D", 3) ];
      ]
      [ "p2" ];
    showdown_test "Three of a Kind vs Three of a Kind Kicker"
      (make_board [ ("D", 2); ("H", 3); ("H", 6); ("C", 2); ("H", 12) ])
      [
        make_player "p1" [ ("S", 2); ("H", 14) ];
        make_player "p2" [ ("H", 2); ("D", 5) ];
      ]
      [ "p1" ];
    showdown_test "Three of a Kind vs Three of a Kind Chop"
      (make_board [ ("D", 2); ("H", 3); ("H", 6); ("C", 2); ("H", 12) ])
      [
        make_player "p1" [ ("S", 2); ("H", 14) ];
        make_player "p2" [ ("H", 2); ("D", 14) ];
      ]
      [ "p1"; "p2" ];
    showdown_test "Ace Straight vs Ace Straight chop"
      (make_board [ ("D", 2); ("H", 3); ("H", 5); ("C", 4); ("H", 12) ])
      [
        make_player "p1" [ ("S", 2); ("H", 14) ];
        make_player "p2" [ ("H", 2); ("D", 14) ];
      ]
      [ "p1"; "p2" ];
    showdown_test "Straight vs Pair"
      (make_board [ ("D", 2); ("H", 3); ("H", 6); ("C", 4); ("H", 12) ])
      [
        make_player "p1" [ ("S", 9); ("H", 5) ];
        make_player "p2" [ ("H", 2); ("D", 14) ];
      ]
      [ "p1" ];
    showdown_test "Straight vs Three of a Kind"
      (make_board [ ("D", 2); ("H", 3); ("H", 6); ("C", 2); ("H", 12) ])
      [
        make_player "p1" [ ("S", 4); ("H", 5) ];
        make_player "p2" [ ("H", 2); ("D", 14) ];
      ]
      [ "p1" ];
    showdown_test "Straight vs Higher Straight"
      (make_board [ ("D", 2); ("H", 3); ("H", 5); ("C", 2); ("H", 12) ])
      [
        make_player "p1" [ ("S", 4); ("H", 6) ];
        make_player "p2" [ ("H", 4); ("D", 14) ];
      ]
      [ "p1" ];
    showdown_test "Straight vs Higher Straight"
      (make_board [ ("H", 6); ("H", 7); ("H", 9); ("H", 10); ("C", 10) ])
      [
        make_player "p1" [ ("D", 8); ("D", 2) ];
        make_player "p2" [ ("S", 8); ("D", 11) ];
      ]
      [ "p2" ];
    showdown_test "Flush vs Three of a Kind"
      (make_board [ ("D", 2); ("H", 8); ("H", 5); ("C", 2); ("H", 12) ])
      [
        make_player "p1" [ ("S", 4); ("H", 2) ];
        make_player "p2" [ ("H", 7); ("H", 14) ];
      ]
      [ "p2" ];
    showdown_test "Straight vs Higher Straight (Ace High)"
      (make_board [ ("H", 6); ("H", 10); ("H", 11); ("H", 12); ("C", 13) ])
      [
        make_player "p1" [ ("D", 9); ("D", 2) ];
        make_player "p2" [ ("S", 8); ("D", 14) ];
      ]
      [ "p2" ];
    showdown_test "Flush vs Flush"
      (make_board [ ("D", 2); ("H", 8); ("H", 5); ("C", 2); ("H", 12) ])
      [
        make_player "p1" [ ("H", 4); ("H", 14) ];
        make_player "p2" [ ("H", 7); ("H", 13) ];
      ]
      [ "p1" ];
    showdown_test "Flush vs Flush chop"
      (make_board [ ("H", 3); ("H", 8); ("H", 5); ("H", 2); ("H", 12) ])
      [
        make_player "p1" [ ("D", 4); ("D", 14) ];
        make_player "p2" [ ("D", 7); ("D", 13) ];
      ]
      [ "p1"; "p2" ];
    showdown_test "Flush vs Flush kickers"
      (make_board [ ("H", 3); ("D", 8); ("H", 5); ("H", 2); ("H", 12) ])
      [
        make_player "p1" [ ("H", 4); ("D", 14) ];
        make_player "p2" [ ("H", 7); ("D", 13) ];
      ]
      [ "p2" ];
    showdown_test "Flush vs Full House"
      (make_board [ ("D", 2); ("H", 8); ("H", 5); ("C", 2); ("H", 12) ])
      [
        make_player "p1" [ ("H", 4); ("H", 14) ];
        make_player "p2" [ ("H", 2); ("C", 12) ];
      ]
      [ "p2" ];
    showdown_test "Full House vs Full House Top Rank"
      (make_board [ ("C", 8); ("H", 8); ("H", 5); ("C", 5); ("H", 12) ])
      [
        make_player "p1" [ ("S", 5); ("H", 14) ];
        make_player "p2" [ ("H", 2); ("D", 8) ];
      ]
      [ "p2" ];
    showdown_test "Full House vs Full House Lower Rank"
      (make_board [ ("C", 8); ("H", 8); ("H", 5); ("C", 5); ("H", 12) ])
      [
        make_player "p1" [ ("S", 8); ("C", 12) ];
        make_player "p2" [ ("H", 2); ("D", 8) ];
      ]
      [ "p1" ];
    showdown_test "Four of a Kind vs Full House"
      (make_board [ ("C", 8); ("H", 8); ("H", 5); ("C", 5); ("H", 12) ])
      [
        make_player "p1" [ ("S", 8); ("C", 12) ];
        make_player "p2" [ ("S", 5); ("D", 5) ];
      ]
      [ "p2" ];
    showdown_test "Four of a Kind vs Four of a Kind chop"
      (make_board [ ("D", 2); ("H", 2); ("S", 2); ("C", 2); ("H", 3) ])
      [
        make_player "p1" [ ("S", 3); ("H", 14) ];
        make_player "p2" [ ("H", 4); ("D", 14) ];
      ]
      [ "p1"; "p2" ];
    showdown_test "Four of a Kind vs Four of a Kind kicker"
      (make_board [ ("D", 2); ("H", 2); ("S", 2); ("C", 2); ("H", 3) ])
      [
        make_player "p1" [ ("S", 3); ("H", 14) ];
        make_player "p2" [ ("H", 4); ("D", 12) ];
      ]
      [ "p1" ];
    showdown_test "Four of a Kind vs Four of a Kind"
      (make_board [ ("C", 8); ("H", 8); ("H", 5); ("C", 5); ("H", 12) ])
      [
        make_player "p1" [ ("S", 8); ("D", 8) ];
        make_player "p2" [ ("S", 5); ("D", 5) ];
      ]
      [ "p1" ];
    showdown_test "Straight Flush vs Two Pair"
      (make_board [ ("H", 6); ("H", 7); ("H", 9); ("H", 10); ("C", 10) ])
      [
        make_player "p1" [ ("H", 8); ("D", 2) ];
        make_player "p2" [ ("S", 5); ("D", 5) ];
      ]
      [ "p1" ];
    showdown_test "Straight Flush vs Higher Straight"
      (make_board [ ("H", 6); ("H", 7); ("H", 9); ("H", 10); ("C", 10) ])
      [
        make_player "p1" [ ("H", 8); ("D", 2) ];
        make_player "p2" [ ("S", 8); ("D", 11) ];
      ]
      [ "p1" ];
    showdown_test "Straight Flush vs Higher Straight Flush"
      (make_board [ ("H", 3); ("H", 4); ("H", 5); ("S", 10); ("C", 10) ])
      [
        make_player "p1" [ ("H", 14); ("H", 2) ];
        make_player "p2" [ ("H", 6); ("H", 7) ];
      ]
      [ "p2" ];
    showdown_test "Straight Flush vs Higher Flush"
      (make_board [ ("D", 8); ("D", 12); ("D", 11); ("D", 10); ("C", 4) ])
      [
        make_player "p1" [ ("D", 9); ("H", 8) ];
        make_player "p2" [ ("D", 14); ("S", 6) ];
      ]
      [ "p1" ];
    showdown_test "Royal Flush vs Straight Flush"
      (make_board [ ("D", 13); ("D", 12); ("D", 11); ("D", 10); ("C", 4) ])
      [
        make_player "p1" [ ("D", 14); ("H", 8) ];
        make_player "p2" [ ("D", 9); ("S", 6) ];
      ]
      [ "p1" ];
    showdown_test "Royal Flush vs Equal Straight"
      (make_board [ ("D", 13); ("D", 12); ("D", 11); ("D", 10); ("C", 4) ])
      [
        make_player "p1" [ ("D", 14); ("H", 8) ];
        make_player "p2" [ ("C", 14); ("S", 6) ];
      ]
      [ "p1" ];
    (*Pseudorandom test cases to verify correctness of showdown.ml*)
    showdown_test "Random test 1"
      (make_board [ ("D", 2); ("S", 8); ("H", 7); ("H", 10); ("C", 5) ])
      [
        make_player "p1" [ ("D", 14); ("H", 13) ];
        make_player "p2" [ ("D", 10); ("D", 3) ];
      ]
      [ "p2" ];
    showdown_test "Random test 2"
      (make_board [ ("D", 13); ("C", 10); ("C", 2); ("S", 2); ("C", 4) ])
      [
        make_player "p1" [ ("C", 3); ("H", 8) ];
        make_player "p2" [ ("S", 9); ("C", 11) ];
      ]
      [ "p2" ];
    showdown_test "Random test 3"
      (make_board [ ("D", 2); ("H", 8); ("D", 11); ("S", 14); ("S", 5) ])
      [
        make_player "p1" [ ("S", 13); ("D", 3) ];
        make_player "p2" [ ("S", 2); ("H", 2) ];
      ]
      [ "p2" ];
    showdown_test "Random test 4"
      (make_board [ ("D", 6); ("H", 8); ("H", 11); ("S", 11); ("S", 9) ])
      [
        make_player "p1" [ ("H", 3); ("C", 10) ];
        make_player "p2" [ ("D", 13); ("S", 5) ];
      ]
      [ "p2" ];
    showdown_test "Random test 5"
      (make_board [ ("H", 2); ("D", 11); ("H", 2); ("C", 12); ("C", 14) ])
      [
        make_player "p1" [ ("S", 12); ("S", 4) ];
        make_player "p2" [ ("C", 14); ("H", 9) ];
      ]
      [ "p2" ];
    showdown_test "Random test 6"
      (make_board [ ("D", 12); ("D", 9); ("H", 12); ("S", 10); ("H", 8) ])
      [
        make_player "p1" [ ("H", 11); ("C", 3) ];
        make_player "p2" [ ("H", 7); ("S", 2) ];
      ]
      [ "p1" ];
    showdown_test "Random test 7"
      (make_board [ ("D", 10); ("H", 11); ("S", 2); ("S", 4); ("C", 10) ])
      [
        make_player "p1" [ ("C", 7); ("H", 2) ];
        make_player "p2" [ ("S", 14); ("D", 8) ];
      ]
      [ "p1" ];
    showdown_test "FAILING Random test 8"
      (make_board [ ("H", 13); ("H", 7); ("C", 4); ("C", 9); ("D", 3) ])
      [
        make_player "p1" [ ("C", 11); ("C", 6) ];
        make_player "p2" [ ("D", 12); ("H", 8) ];
      ]
      [ "p2" ];
    showdown_test "Random test 9"
      (make_board [ ("H", 12); ("C", 7); ("H", 4); ("D", 4); ("S", 14) ])
      [
        make_player "p1" [ ("C", 9); ("S", 8) ];
        make_player "p2" [ ("H", 13); ("S", 8) ];
      ]
      [ "p2" ];
    showdown_test "Random test 10"
      (make_board [ ("H", 4); ("H", 2); ("S", 5); ("D", 11); ("D", 12) ])
      [
        make_player "p1" [ ("H", 14); ("H", 8) ];
        make_player "p2" [ ("S", 13); ("C", 11) ];
      ]
      [ "p2" ];
    showdown_test "Random test 11"
      (make_board [ ("H", 4); ("S", 5); ("S", 9); ("C", 13); ("C", 5) ])
      [
        make_player "p1" [ ("C", 3); ("S", 4) ];
        make_player "p2" [ ("H", 9); ("D", 2) ];
      ]
      [ "p2" ];
    showdown_test "Random test 12"
      (make_board [ ("H", 8); ("D", 13); ("S", 12); ("D", 11); ("C", 3) ])
      [
        make_player "p1" [ ("H", 2); ("D", 4) ];
        make_player "p2" [ ("S", 13); ("S", 9) ];
      ]
      [ "p2" ];
    showdown_test "Random test 13"
      (make_board [ ("S", 4); ("S", 14); ("H", 5); ("C", 11); ("D", 10) ])
      [
        make_player "p1" [ ("D", 13); ("C", 6) ];
        make_player "p2" [ ("D", 14); ("C", 9) ];
      ]
      [ "p2" ];
    showdown_test "Random test 14"
      (make_board [ ("H", 8); ("D", 3); ("S", 12); ("S", 13); ("H", 9) ])
      [
        make_player "p1" [ ("C", 13); ("C", 6) ];
        make_player "p2" [ ("S", 13); ("H", 5) ];
      ]
      [ "p1"; "p2" ];
    showdown_test "Random test 15"
      (make_board [ ("H", 7); ("D", 8); ("S", 3); ("S", 9); ("D", 5) ])
      [
        make_player "p1" [ ("D", 6); ("C", 12) ];
        make_player "p2" [ ("S", 2); ("S", 8) ];
      ]
      [ "p1" ];
    showdown_test "Random test 16"
      (make_board [ ("H", 12); ("S", 14); ("D", 9); ("S", 3); ("C", 7) ])
      [
        make_player "p1" [ ("S", 12); ("C", 3) ];
        make_player "p2" [ ("S", 11); ("D", 13) ];
      ]
      [ "p1" ];
    showdown_test "Random test 17"
      (make_board [ ("H", 12); ("S", 3); ("S", 6); ("D", 12); ("D", 4) ])
      [
        make_player "p1" [ ("D", 9); ("C", 8) ];
        make_player "p2" [ ("H", 12); ("C", 9) ];
      ]
      [ "p2" ];
    showdown_test "Random test 18"
      (make_board [ ("H", 4); ("D", 8); ("H", 9); ("S", 3); ("H", 12) ])
      [
        make_player "p1" [ ("H", 13); ("H", 10) ];
        make_player "p2" [ ("D", 14); ("C", 2) ];
      ]
      [ "p1" ];
    showdown_test "Random test 19"
      (make_board [ ("H", 12); ("D", 4); ("H", 9); ("S", 9); ("S", 2) ])
      [
        make_player "p1" [ ("S", 13); ("C", 11) ];
        make_player "p2" [ ("D", 11); ("S", 14) ];
      ]
      [ "p2" ];
    showdown_test "Random test 20"
      (make_board [ ("S", 12); ("C", 2); ("D", 13); ("C", 7); ("D", 5) ])
      [
        make_player "p1" [ ("S", 8); ("C", 8) ];
        make_player "p2" [ ("S", 7); ("S", 10) ];
      ]
      [ "p1" ];
    showdown_test "Random test 21"
      (make_board [ ("S", 10); ("C", 13); ("S", 2); ("H", 4); ("S", 5) ])
      [
        make_player "p1" [ ("C", 14); ("D", 6) ];
        make_player "p2" [ ("D", 9); ("H", 10) ];
      ]
      [ "p2" ];
    showdown_test "Random test 22"
      (make_board [ ("D", 7); ("H", 9); ("S", 14); ("S", 4); ("D", 4) ])
      [
        make_player "p1" [ ("S", 14); ("D", 9) ];
        make_player "p2" [ ("S", 7); ("H", 6) ];
      ]
      [ "p1" ];
    showdown_test "Random test 23"
      (make_board [ ("S", 7); ("S", 2); ("C", 12); ("D", 11); ("C", 8) ])
      [
        make_player "p1" [ ("H", 8); ("D", 4) ];
        make_player "p2" [ ("S", 10); ("C", 7) ];
      ]
      [ "p1" ];
    showdown_test "Random test 24"
      (make_board [ ("C", 4); ("C", 13); ("S", 11); ("D", 6); ("D", 9) ])
      [
        make_player "p1" [ ("H", 13); ("S", 6) ];
        make_player "p2" [ ("C", 3); ("C", 7) ];
      ]
      [ "p1" ];
    showdown_test "Random test 25"
      (make_board [ ("C", 12); ("H", 14); ("D", 5); ("D", 7); ("S", 12) ])
      [
        make_player "p1" [ ("S", 8); ("H", 7) ];
        make_player "p2" [ ("C", 3); ("C", 4) ];
      ]
      [ "p1" ];
    showdown_test "Random test 26"
      (make_board [ ("H", 14); ("C", 8); ("C", 4); ("S", 11); ("S", 9) ])
      [
        make_player "p1" [ ("S", 10); ("C", 7) ];
        make_player "p2" [ ("D", 11); ("H", 12) ];
      ]
      [ "p1" ];
    showdown_test "Random test 27"
      (make_board [ ("D", 2); ("H", 5); ("S", 7); ("C", 10); ("S", 10) ])
      [
        make_player "p1" [ ("H", 3); ("S", 5) ];
        make_player "p2" [ ("C", 8); ("D", 9) ];
      ]
      [ "p1" ];
    showdown_test "Random test 28"
      (make_board [ ("H", 10); ("D", 3); ("S", 8); ("S", 11); ("S", 12) ])
      [
        make_player "p1" [ ("H", 9); ("C", 6) ];
        make_player "p2" [ ("H", 13); ("D", 2) ];
      ]
      [ "p1" ];
    showdown_test "Random test 29"
      (make_board [ ("S", 9); ("D", 6); ("C", 6); ("H", 3); ("D", 8) ])
      [
        make_player "p1" [ ("C", 14); ("C", 13) ];
        make_player "p2" [ ("S", 5); ("H", 5) ];
      ]
      [ "p2" ];
    showdown_test "Random test 30"
      (make_board [ ("C", 13); ("S", 2); ("C", 4); ("S", 8); ("D", 4) ])
      [
        make_player "p1" [ ("D", 14); ("H", 8) ];
        make_player "p2" [ ("C", 14); ("S", 6) ];
      ]
      [ "p1" ];
    showdown_test "Random test 31"
      (make_board [ ("H", 2); ("C", 5); ("S", 11); ("D", 3); ("D", 4) ])
      [
        make_player "p1" [ ("D", 2); ("H", 5) ];
        make_player "p2" [ ("C", 9); ("S", 10) ];
      ]
      [ "p1" ];
    showdown_test "Random test 32"
      (make_board [ ("S", 3); ("D", 3); ("H", 4); ("D", 12); ("C", 3) ])
      [
        make_player "p1" [ ("S", 13); ("C", 11) ];
        make_player "p2" [ ("D", 11); ("S", 14) ];
      ]
      [ "p2" ];
    showdown_test "Random test 33"
      (make_board [ ("C", 4); ("H", 4); ("D", 5); ("D", 6); ("H", 5) ])
      [
        make_player "p1" [ ("D", 2); ("S", 5) ];
        make_player "p2" [ ("C", 9); ("S", 10) ];
      ]
      [ "p1" ];
    showdown_test "Random test 34"
      (make_board [ ("H", 8); ("S", 10); ("S", 10); ("D", 6); ("S", 14) ])
      [
        make_player "p1" [ ("C", 9); ("S", 8) ];
        make_player "p2" [ ("H", 13); ("S", 8) ];
      ]
      [ "p1"; "p2" ];
    showdown_test "Random test 35"
      (make_board [ ("C", 9); ("C", 5); ("H", 7); ("D", 11); ("D", 9) ])
      [
        make_player "p1" [ ("C", 3); ("S", 4) ];
        make_player "p2" [ ("H", 9); ("D", 2) ];
      ]
      [ "p2" ];
    showdown_test "Random test 36"
      (make_board [ ("S", 10); ("C", 4); ("D", 10); ("D", 13); ("H", 10) ])
      [
        make_player "p1" [ ("D", 2); ("H", 5) ];
        make_player "p2" [ ("C", 9); ("S", 10) ];
      ]
      [ "p2" ];
    showdown_test "Random test 37"
      (make_board [ ("D", 5); ("H", 14); ("S", 3); ("D", 5); ("C", 12) ])
      [
        make_player "p1" [ ("D", 14); ("H", 8) ];
        make_player "p2" [ ("C", 14); ("S", 6) ];
      ]
      [ "p1"; "p2" ];
    showdown_test "Random test 38"
      (make_board [ ("H", 7); ("D", 10); ("S", 13); ("D", 14); ("D", 10) ])
      [
        make_player "p1" [ ("C", 3); ("S", 4) ];
        make_player "p2" [ ("H", 9); ("D", 2) ];
      ]
      [ "p2" ];
    showdown_test "Random test 39"
      (make_board [ ("S", 14); ("C", 9); ("C", 12); ("D", 12); ("H", 3) ])
      [
        make_player "p1" [ ("C", 9); ("S", 8) ];
        make_player "p2" [ ("H", 13); ("S", 8) ];
      ]
      [ "p1" ];
    showdown_test "Random test 40"
      (make_board [ ("C", 13); ("S", 6); ("H", 6); ("D", 10); ("H", 6) ])
      [
        make_player "p1" [ ("S", 13); ("C", 11) ];
        make_player "p2" [ ("D", 11); ("S", 14) ];
      ]
      [ "p1" ];
    showdown_test "Random test 41"
      (make_board [ ("S", 3); ("H", 12); ("C", 13); ("D", 9); ("S", 9) ])
      [
        make_player "p1" [ ("H", 13); ("H", 10) ];
        make_player "p2" [ ("D", 14); ("C", 2) ];
      ]
      [ "p1" ];
    showdown_test "Random test 42"
      (make_board [ ("D", 2); ("C", 8); ("D", 3); ("D", 10); ("S", 7) ])
      [
        make_player "p1" [ ("C", 9); ("S", 8) ];
        make_player "p2" [ ("H", 13); ("S", 8) ];
      ]
      [ "p2" ];
    showdown_test "Random test 43"
      (make_board [ ("C", 3); ("H", 12); ("S", 2); ("D", 3); ("H", 4) ])
      [
        make_player "p1" [ ("D", 14); ("H", 8) ];
        make_player "p2" [ ("C", 14); ("S", 6) ];
      ]
      [ "p1" ];
    showdown_test "Random test 44"
      (make_board [ ("S", 9); ("C", 5); ("C", 7); ("D", 14); ("C", 6) ])
      [
        make_player "p1" [ ("C", 3); ("S", 4) ];
        make_player "p2" [ ("H", 9); ("D", 2) ];
      ]
      [ "p1" ];
    showdown_test "Random test 45"
      (make_board [ ("C", 10); ("C", 5); ("H", 8); ("D", 12); ("D", 5) ])
      [
        make_player "p1" [ ("S", 13); ("C", 11) ];
        make_player "p2" [ ("D", 11); ("S", 14) ];
      ]
      [ "p2" ];
    showdown_test "Random test 46"
      (make_board [ ("D", 11); ("D", 8); ("D", 6); ("D", 3); ("S", 11) ])
      [
        make_player "p1" [ ("C", 9); ("S", 8) ];
        make_player "p2" [ ("H", 13); ("D", 8) ];
      ]
      [ "p2" ];
    showdown_test "Random test 47"
      (make_board [ ("H", 12); ("D", 2); ("S", 6); ("D", 14); ("C", 11) ])
      [
        make_player "p1" [ ("H", 13); ("H", 10) ];
        make_player "p2" [ ("D", 14); ("C", 2) ];
      ]
      [ "p1" ];
    showdown_test "Random test 48"
      (make_board [ ("H", 3); ("H", 12); ("S", 10); ("D", 9); ("H", 14) ])
      [
        make_player "p1" [ ("S", 13); ("C", 11) ];
        make_player "p2" [ ("D", 11); ("S", 14) ];
      ]
      [ "p1" ];
    showdown_test "Random test 49"
      (make_board [ ("S", 7); ("S", 9); ("C", 5); ("D", 2); ("H", 5) ])
      [
        make_player "p1" [ ("D", 14); ("H", 8) ];
        make_player "p2" [ ("C", 14); ("S", 6) ];
      ]
      [ "p1" ];
    showdown_test "Random test 50"
      (make_board [ ("D", 8); ("D", 10); ("S", 4); ("D", 5); ("S", 7) ])
      [
        make_player "p1" [ ("C", 3); ("S", 4) ];
        make_player "p2" [ ("H", 9); ("D", 2) ];
      ]
      [ "p1" ];
    showdown_test "Random test 51"
      (make_board [ ("S", 4); ("H", 12); ("H", 5); ("D", 3); ("D", 6) ])
      [
        make_player "p1" [ ("S", 13); ("C", 11) ];
        make_player "p2" [ ("D", 11); ("S", 14) ];
      ]
      [ "p2" ];
    showdown_test "Random test 52"
      (make_board [ ("D", 6); ("D", 5); ("S", 7); ("D", 9); ("C", 10) ])
      [
        make_player "p1" [ ("C", 9); ("S", 8) ];
        make_player "p2" [ ("H", 13); ("S", 8) ];
      ]
      [ "p1"; "p2" ];
    showdown_test "Random test 53"
      (make_board [ ("D", 9); ("D", 8); ("H", 7); ("D", 10); ("S", 11) ])
      [
        make_player "p1" [ ("D", 2); ("H", 5) ];
        make_player "p2" [ ("C", 9); ("S", 10) ];
      ]
      [ "p1"; "p2" ];
    showdown_test "Random test 54"
      (make_board [ ("C", 11); ("H", 14); ("C", 13); ("D", 5); ("D", 4) ])
      [
        make_player "p1" [ ("C", 3); ("S", 4) ];
        make_player "p2" [ ("H", 9); ("D", 2) ];
      ]
      [ "p1" ];
    showdown_test "Random test 55"
      (make_board [ ("C", 14); ("D", 3); ("C", 4); ("D", 8); ("C", 3) ])
      [
        make_player "p1" [ ("D", 14); ("H", 8) ];
        make_player "p2" [ ("C", 14); ("S", 6) ];
      ]
      [ "p1" ];
    showdown_test "Random test 56"
      (make_board [ ("S", 6); ("H", 13); ("H", 11); ("D", 9); ("H", 2) ])
      [
        make_player "p1" [ ("D", 2); ("H", 5) ];
        make_player "p2" [ ("C", 9); ("S", 10) ];
      ]
      [ "p2" ];
    showdown_test "Random test 57"
      (make_board [ ("H", 8); ("C", 10); ("H", 5); ("D", 8); ("D", 6) ])
      [
        make_player "p1" [ ("H", 13); ("H", 10) ];
        make_player "p2" [ ("D", 14); ("C", 2) ];
      ]
      [ "p1" ];
    showdown_test "Random test 58"
      (make_board [ ("S", 10); ("S", 11); ("C", 11); ("D", 9); ("C", 12) ])
      [
        make_player "p1" [ ("C", 3); ("S", 4) ];
        make_player "p2" [ ("H", 9); ("D", 2) ];
      ]
      [ "p2" ];
    showdown_test "Random test 59"
      (make_board [ ("C", 2); ("D", 5); ("H", 5); ("D", 5); ("S", 13) ])
      [
        make_player "p1" [ ("D", 14); ("H", 8) ];
        make_player "p2" [ ("C", 14); ("S", 6) ];
      ]
      [ "p1"; "p2" ];
    showdown_test "Random test 60"
      (make_board [ ("D", 12); ("D", 7); ("C", 9); ("D", 2); ("H", 14) ])
      [
        make_player "p1" [ ("S", 13); ("C", 11) ];
        make_player "p2" [ ("D", 11); ("S", 14) ];
      ]
      [ "p2" ];
    showdown_test "Random test 61"
      (make_board [ ("H", 14); ("C", 13); ("H", 4); ("D", 7); ("H", 12) ])
      [
        make_player "p1" [ ("C", 3); ("S", 4) ];
        make_player "p2" [ ("H", 9); ("D", 2) ];
      ]
      [ "p1" ];
    showdown_test "Random test 62"
      (make_board [ ("D", 13); ("D", 14); ("C", 13); ("D", 10); ("S", 8) ])
      [
        make_player "p1" [ ("H", 13); ("H", 10) ];
        make_player "p2" [ ("D", 14); ("C", 2) ];
      ]
      [ "p1" ];
    showdown_test "Random test 63"
      (make_board [ ("S", 5); ("S", 9); ("S", 6); ("D", 8); ("C", 9) ])
      [
        make_player "p1" [ ("C", 9); ("S", 8) ];
        make_player "p2" [ ("H", 13); ("S", 8) ];
      ]
      [ "p1" ];
    showdown_test "Random test 64"
      (make_board [ ("H", 6); ("H", 4); ("D", 2); ("D", 10); ("D", 12) ])
      [
        make_player "p1" [ ("S", 13); ("C", 11) ];
        make_player "p2" [ ("D", 11); ("S", 14) ];
      ]
      [ "p2" ];
    showdown_test "Random test 65"
      (make_board [ ("C", 3); ("S", 10); ("C", 13); ("D", 11); ("H", 5) ])
      [
        make_player "p1" [ ("C", 3); ("S", 4) ];
        make_player "p2" [ ("H", 9); ("D", 2) ];
      ]
      [ "p1" ];
    showdown_test "Random test 66"
      (make_board [ ("D", 10); ("H", 5); ("H", 7); ("D", 8); ("D", 7) ])
      [
        make_player "p1" [ ("D", 14); ("H", 8) ];
        make_player "p2" [ ("C", 14); ("S", 6) ];
      ]
      [ "p1" ];
    showdown_test "Random test 67"
      (make_board [ ("H", 12); ("C", 14); ("D", 12); ("D", 13); ("S", 2) ])
      [
        make_player "p1" [ ("S", 13); ("C", 11) ];
        make_player "p2" [ ("D", 11); ("S", 14) ];
      ]
      [ "p2" ];
    showdown_test "Random test 68"
      (make_board [ ("D", 13); ("D", 2); ("S", 3); ("D", 3); ("C", 8) ])
      [
        make_player "p1" [ ("C", 3); ("S", 4) ];
        make_player "p2" [ ("H", 9); ("D", 2) ];
      ]
      [ "p1" ];
    showdown_test "Random test 69"
      (make_board [ ("C", 14); ("D", 9); ("H", 10); ("D", 7); ("S", 10) ])
      [
        make_player "p1" [ ("C", 9); ("S", 8) ];
        make_player "p2" [ ("H", 13); ("S", 8) ];
      ]
      [ "p1" ];
    showdown_test "Random test 70"
      (make_board [ ("S", 11); ("S", 10); ("H", 8); ("D", 2); ("C", 6) ])
      [
        make_player "p1" [ ("S", 13); ("C", 11) ];
        make_player "p2" [ ("D", 11); ("S", 14) ];
      ]
      [ "p2" ];
    showdown_test "Random test 71"
      (make_board [ ("H", 7); ("S", 3); ("S", 8); ("D", 13); ("D", 4) ])
      [
        make_player "p1" [ ("C", 3); ("S", 4) ];
        make_player "p2" [ ("H", 9); ("D", 2) ];
      ]
      [ "p1" ];
    showdown_test "Random test 72"
      (make_board [ ("S", 8); ("C", 12); ("S", 9); ("D", 6); ("H", 6) ])
      [
        make_player "p1" [ ("D", 2); ("H", 5) ];
        make_player "p2" [ ("C", 9); ("S", 10) ];
      ]
      [ "p2" ];
    showdown_test "Random test 73"
      (make_board [ ("D", 13); ("D", 3); ("D", 6); ("D", 8); ("S", 10) ])
      [
        make_player "p1" [ ("C", 3); ("S", 4) ];
        make_player "p2" [ ("H", 9); ("D", 2) ];
      ]
      [ "p2" ];
    showdown_test "Random test 74"
      (make_board [ ("H", 14); ("S", 9); ("C", 3); ("D", 5); ("H", 14) ])
      [
        make_player "p1" [ ("H", 13); ("H", 10) ];
        make_player "p2" [ ("D", 14); ("C", 2) ];
      ]
      [ "p2" ];
    showdown_test "Random test 75"
      (make_board [ ("D", 5); ("H", 2); ("S", 11); ("D", 11); ("D", 6) ])
      [
        make_player "p1" [ ("S", 13); ("C", 11) ];
        make_player "p2" [ ("D", 11); ("S", 14) ];
      ]
      [ "p2" ];
    showdown_test "Random test 76"
      (make_board [ ("C", 7); ("H", 4); ("D", 6); ("D", 5); ("C", 9) ])
      [
        make_player "p1" [ ("C", 9); ("S", 8) ];
        make_player "p2" [ ("H", 13); ("S", 8) ];
      ]
      [ "p1"; "p2" ];
    showdown_test "Random test 77"
      (make_board [ ("S", 6); ("D", 14); ("C", 3); ("D", 8); ("S", 5) ])
      [
        make_player "p1" [ ("C", 3); ("S", 4) ];
        make_player "p2" [ ("H", 9); ("D", 2) ];
      ]
      [ "p1" ];
    showdown_test "Random test 78"
      (make_board [ ("C", 8); ("S", 11); ("D", 5); ("D", 6); ("C", 3) ])
      [
        make_player "p1" [ ("H", 13); ("H", 10) ];
        make_player "p2" [ ("D", 14); ("C", 2) ];
      ]
      [ "p2" ];
    showdown_test "Random test 79"
      (make_board [ ("D", 4); ("S", 10); ("C", 7); ("D", 8); ("D", 3) ])
      [
        make_player "p1" [ ("D", 2); ("H", 5) ];
        make_player "p2" [ ("C", 9); ("S", 10) ];
      ]
      [ "p2" ];
    showdown_test "Random test 80"
      (make_board [ ("H", 10); ("S", 12); ("H", 13); ("D", 13); ("H", 4) ])
      [
        make_player "p1" [ ("S", 13); ("C", 11) ];
        make_player "p2" [ ("D", 11); ("S", 14) ];
      ]
      [ "p2" ];
  ]

(******************************************************************
    Test Suite
 ******************************************************************)

let suite =
  "test suite for 3110_final_project"
  >::: List.flatten [ holdem_tests; state_tests; command_tests; showdown_tests ]

let _ = run_test_tt_main suite