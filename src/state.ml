open Holdem

type t = {
  deck : card list;
  players : player list;
  pot : int;
  buy_in : int;
  board : card list;
}

type result =
  | Legal of t
  | Illegal

let init buy_in =
  Legal { deck = shuffled_deck; players = []; pot = 0; buy_in; board = [] }

let turn command player amount st =
  raise (Failure "deal, call, raise, check, and fold")

let edit name st =
  let p = Holdem.make_player name st.buy_in in
  Legal
    {
      deck = st.deck;
      players = p :: st.players;
      pot = st.pot;
      buy_in = st.buy_in;
      board = st.board;
    }

let find_highest_amount (players : player list) =
  List.fold_left max 0 (List.map (fun p -> p.balance) players)

let player_names_to_string (players : player list) =
  String.concat ", " (List.map (fun p -> p.name) players)

let quit st =
  let amt = find_highest_amount st.players in
  player_names_to_string (List.filter (fun p -> p.balance = amt) st.players)
  ^ " won with an amount of " ^ string_of_int amt

let rec players_to_string players =
  match players with
  | [ h ] -> player_to_string h ^ "\n"
  | h :: t -> player_to_string h ^ "\n" ^ players_to_string t
  | [] -> "No current players\n"

let rec repeat_string n str =
  if n = 0 then "" else str ^ repeat_string (n - 1) str

let unknown_cards_to_string (board : card list) =
  let n = 5 - List.length board in
  repeat_string n " __"

let state_to_string st =
  "TABLE:\n"
  ^ players_to_string st.players
  ^ "Pot: " ^ string_of_int st.pot ^ " Chips\n" ^ "Board:"
  ^ cards_to_string st.board
  ^ unknown_cards_to_string st.board
