open Holdem

type t = {
  deck : card list;
  players : player list;
  pot : int;
  buy_in : int;
  board : card list;
}

let init buy_in =
  { deck = shuffled_deck; players = []; pot = 0; buy_in; board = [] }

let turn command player amount st =
  raise (Failure "deal, call, raise, check, and fold")

let edit name st =
  let p = Holdem.make_player name st.buy_in in
  {
    deck = st.deck;
    players = p :: st.players;
    pot = st.pot;
    buy_in = st.buy_in;
    board = st.board;
  }

let find_winner (players : player list) =
  List.fold_left max 0 (List.map (fun p -> p.balance) players)

let players_to_string (players : player list) =
  String.concat ", " (List.map (fun p -> p.name) players)

let quit st =
  let amt = find_winner st.players in
  players_to_string (List.filter (fun p -> p.balance = amt) st.players)
  ^ " won with an amount of " ^ string_of_int amt

let rec print_players players =
  match players with
  | [ h ] -> print_endline (player_to_string h)
  | h :: t ->
      print_endline (player_to_string h);
      print_players t
  | [] -> print_endline "No current players"

let print st =
  print_endline "TABLE:";
  print_players st.players;
  print_string "Pot: ";
  print_int st.pot;
  print_endline "Chips";
  print_string "Board: ";
  print_string (cards_to_string st.board);
  for x = List.length st.board to 4 do
    print_string "__"
  done
