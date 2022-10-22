open Holdem

type t = {
  deck : card list;
  players : player list;
  pot : int;
  buy_in : int;
  board : card list;
  active : bool;
}

type result =
  | Legal of t
  | Illegal of string

let init buy_in =
  {
    deck = shuffled_deck ();
    players = [];
    pot = 0;
    buy_in;
    board = [];
    active = false;
  }

let deal_to_player p st =
  let card = Holdem.top_card st.deck in
  let player = Holdem.deal_to p card in
  let players =
    List.map (fun pl1 -> if pl1 = p then player else pl1) st.players
  in
  {
    deck = Holdem.draw_from_deck st.deck;
    players;
    pot = st.pot;
    buy_in = st.buy_in;
    board = st.board;
    active = true;
  }

let rec deal i st =
  if List.length st.players <= i then st
  else
    let p = List.nth st.players i in
    deal_to_player p st |> deal_to_player p |> deal (i + 1)

let call st = Illegal "Unimplemented"
let check st = Illegal "Unimplemented"
let fold st = Illegal "Unimplemented"
let raise st = Illegal "Unimplemented"

let add name st =
  if List.exists (fun p -> p.name = name) st.players then
    Illegal "Error: Name already being used!\n"
  else
    let p = Holdem.make_player name st.buy_in in
    Legal
      {
        deck = st.deck;
        players = p :: st.players;
        pot = st.pot;
        buy_in = st.buy_in;
        board = st.board;
        active = false;
      }

let remove name st =
  if List.exists (fun p -> p.name = name) st.players then
    Legal
      {
        deck = st.deck;
        players = List.filter (fun p -> p.name <> name) st.players;
        pot = st.pot;
        buy_in = st.buy_in;
        board = st.board;
        active = false;
      }
  else Illegal "Error: Name does not exist in list of players!\n"

let action cmd (st : t) : result =
  match cmd with
  | Command.Deal ->
      if st.active then Illegal "The cards have already been dealt\n"
      else Legal (deal 0 st)
  | Command.Call ->
      if st.active then Illegal "The cards hae not been dealt yet\n"
      else call st
  | Command.Check ->
      if st.active then Illegal "The cards hae not been dealt yet\n"
      else check st
  | Command.Fold ->
      if st.active then Illegal "The cards hae not been dealt yet\n"
      else fold st
  | Command.Raise i ->
      if st.active then Illegal "The cards hae not been dealt yet\n"
      else raise st
  | Command.AddPlayer name ->
      if st.active then Illegal "Players cannot be added mid-round\n"
      else add name st
  | Command.RemovePlayer name ->
      if st.active then Illegal "Players cannot be added mid-round\n"
      else remove name st

let find_highest_amount (players : player list) =
  List.fold_left max 0 (List.map (fun p -> p.balance) players)

let player_names_to_string (players : player list) =
  String.concat ", " (List.map (fun p -> p.name) players)

let quit st =
  let amt = find_highest_amount st.players in
  player_names_to_string (List.filter (fun p -> p.balance = amt) st.players)
  ^ " won with an amount of " ^ string_of_int amt ^ "\n\n"

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
  ^ "\n"