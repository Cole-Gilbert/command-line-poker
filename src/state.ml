open Holdem

type t = {
  deck : card list;
  players : player list;
  pot : int;
  buy_in : int;
  board : card list;
  active : bool;
  position : int;
  min_bet : int;
  confirmed : bool;
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
    position = 0;
    min_bet = buy_in;
    confirmed = false;
  }

let comfirm st =
  if (not st.active) || st.confirmed then
    Illegal "Error: Please Enter a Command\n"
  else
    Legal
      {
        deck = st.deck;
        players = st.players;
        pot = st.pot;
        buy_in = st.buy_in;
        board = st.board;
        active = st.active;
        position = st.position;
        min_bet = st.min_bet;
        confirmed = true;
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
    position = st.position;
    min_bet = st.min_bet;
    confirmed = st.confirmed;
  }

let deal st =
  if st.active then Illegal "Error: The cards have already been dealt\n"
  else if List.length st.players = 0 then
    Illegal "Error: There are no players to deal to\n"
  else Legal (List.fold_left (fun st p -> deal_to_player p st) st st.players)

let call st =
  if not st.active then Illegal "Error: The cards have not been dealt yet\n"
  else Illegal "Error: Unimplemented\n"

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
        active = st.active;
        position = st.position;
        min_bet = st.min_bet;
        confirmed = st.confirmed;
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
        active = st.active;
        position = st.position;
        min_bet = st.min_bet;
        confirmed = st.confirmed;
      }
  else Illegal "Error: Name does not exist in list of players!\n"

let action cmd (st : t) : result =
  match cmd with
  | Command.Comfirm -> comfirm st
  | Command.Deal -> deal st
  | Command.Call -> call st
  | Command.Check -> check st
  | Command.Fold -> fold st
  | Command.Raise i -> raise st
  | Command.AddPlayer name -> add name st
  | Command.RemovePlayer name -> remove name st

let quit st =
  let amt = List.fold_left max 0 (List.map (fun p -> p.balance) st.players) in
  let winners = List.filter (fun p -> p.balance = amt) st.players in
  let player_names = String.concat ", " (List.map (fun p -> p.name) winners) in
  player_names ^ " won with an amount of " ^ string_of_int amt ^ "\n\n"

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
  ^ "\n\n"
  ^
  if st.active then
    let player = List.nth st.players st.position in
    if st.confirmed then revealed_player_to_string player
    else
      Holdem.player_to_string player
      ^ ", are you ready? Press enter to start your turn."
  else "You can add/remove players or deal cards"
