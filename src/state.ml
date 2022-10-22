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

let deal_to_player (p : player) st =
  let card1 = Holdem.top_card st.deck in
  let deck1 = Holdem.draw_from_deck st.deck in
  let card2 = Holdem.top_card deck1 in
  let player = Holdem.deal_to card1 p |> Holdem.deal_to card2 in
  let players =
    List.map (fun pl1 -> if pl1 = p then player else pl1) st.players
  in
  {
    deck = Holdem.draw_from_deck deck1;
    players;
    pot = st.pot;
    buy_in = st.buy_in;
    board = st.board;
    active = true;
  }

let rec deal_to_players i st =
  if List.length st.players <= i then st
  else
    let p = List.nth st.players i in
    deal_to_player p st |> deal_to_players (i + 1)

let deal st =
  if st.active then Illegal "Error: The cards have already been dealt\n"
  else if List.length st.players = 0 then
    Illegal "Error: There are no players to deal to\n"
  else Legal (deal_to_players 0 st)

let call st =
  if not st.active then Illegal "Error: The cards have not been dealt yet\n"
  else
    let () =
      print_string (card_to_string (List.nth (List.hd st.players).hand 1))
    in
    Illegal "Error: Unimplemented\n"

let check st =
  if not st.active then Illegal "Error: The cards have not been dealt yet\n"
  else Illegal "Error: Unimplemented\n"

let fold st =
  if not st.active then Illegal "Error: The cards have not been dealt yet\n"
  else Illegal "Error: Unimplemented\n"

let raise st =
  if not st.active then Illegal "Error: The cards have not been dealt yet\n"
  else Illegal "Error: Unimplemented\n"

let add name st =
  if st.active then Illegal "Error: Players cannot be added mid-round\n"
  else if List.exists (fun p -> p.name = name) st.players then
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
  if st.active then Illegal "Error: Players cannot be added mid-round\n"
  else if List.exists (fun p -> p.name = name) st.players then
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
  | Command.Deal -> deal st
  | Command.Call -> call st
  | Command.Check -> check st
  | Command.Fold -> fold st
  | Command.Raise i -> raise st
  | Command.AddPlayer name -> add name st
  | Command.RemovePlayer name -> remove name st

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
  | [ h ] -> Holdem.player_to_string h ^ "\n"
  | h :: t -> Holdem.player_to_string h ^ "\n" ^ players_to_string t
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