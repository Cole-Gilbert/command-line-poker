(*Module representing static holdem data.*)

type suit =
  | Spades
  | Hearts
  | Diamonds
  | Clubs

type card = {
  suit : suit;
  rank : int;
}

type player = {
  name : string;
  balance : int;
  betting : int;
  active : bool;
  hand : card list;
}

let make_player name balance =
  { name; balance; betting = 0; active = true; hand = [] }

let deal_to card player =
  {
    name = player.name;
    balance = player.balance;
    betting = player.betting;
    active = player.active;
    hand = card :: player.hand;
  }

let pay_amount amt player =
  {
    name = player.name;
    balance = player.balance - amt;
    betting = player.betting;
    active = player.active;
    hand = player.hand;
  }

(*For testing purposes*)
let fresh_deck =
  [
    (*Spades A -> K*)
    { suit = Spades; rank = 14 };
    { suit = Spades; rank = 3 };
    { suit = Spades; rank = 4 };
    { suit = Spades; rank = 2 };
    { suit = Spades; rank = 5 };
    { suit = Spades; rank = 6 };
    { suit = Spades; rank = 7 };
    { suit = Spades; rank = 8 };
    { suit = Spades; rank = 9 };
    { suit = Spades; rank = 10 };
    { suit = Spades; rank = 11 };
    { suit = Spades; rank = 12 };
    { suit = Spades; rank = 13 };
    (*Hearts A -> K*)
    { suit = Hearts; rank = 14 };
    { suit = Hearts; rank = 2 };
    { suit = Hearts; rank = 3 };
    { suit = Hearts; rank = 4 };
    { suit = Hearts; rank = 5 };
    { suit = Hearts; rank = 6 };
    { suit = Hearts; rank = 7 };
    { suit = Hearts; rank = 8 };
    { suit = Hearts; rank = 9 };
    { suit = Hearts; rank = 10 };
    { suit = Hearts; rank = 11 };
    { suit = Hearts; rank = 12 };
    { suit = Hearts; rank = 13 };
    (*Diamonds A -> K*)
    { suit = Diamonds; rank = 14 };
    { suit = Diamonds; rank = 2 };
    { suit = Diamonds; rank = 3 };
    { suit = Diamonds; rank = 4 };
    { suit = Diamonds; rank = 5 };
    { suit = Diamonds; rank = 6 };
    { suit = Diamonds; rank = 7 };
    { suit = Diamonds; rank = 8 };
    { suit = Diamonds; rank = 9 };
    { suit = Diamonds; rank = 10 };
    { suit = Diamonds; rank = 11 };
    { suit = Diamonds; rank = 12 };
    { suit = Diamonds; rank = 13 };
    (*Clubs A -> K*)
    { suit = Clubs; rank = 14 };
    { suit = Clubs; rank = 2 };
    { suit = Clubs; rank = 3 };
    { suit = Clubs; rank = 4 };
    { suit = Clubs; rank = 5 };
    { suit = Clubs; rank = 6 };
    { suit = Clubs; rank = 7 };
    { suit = Clubs; rank = 8 };
    { suit = Clubs; rank = 9 };
    { suit = Clubs; rank = 10 };
    { suit = Clubs; rank = 11 };
    { suit = Clubs; rank = 12 };
    { suit = Clubs; rank = 13 };
  ]

(**[active_deck_arr] is the card array representing the current deck.*)
let active_deck_arr = Array.of_list fresh_deck

(**[swap i j arr] modifies the array [arr] by swapping the element with index
   [i] in the array [arr] with the element with index [j] in that same array
   [arr].*)
let swap i j arr =
  let temp = arr.(i) in
  arr.(i) <- arr.(j);
  arr.(j) <- temp

(**[shuffle deck] modifies the card array [deck] by randomizing the locations of
   the cards in the original card aray [deck]. Requires: [deck] has size 52.*)
let shuffle deck =
  for index = 51 downto 1 do
    let random_index = Random.int (index + 1) in
    swap index random_index deck
  done

let shuffled_deck () =
  shuffle active_deck_arr;
  Array.to_list active_deck_arr

let compare card1 card2 = card1.rank - card2.rank
let top_card deck = List.hd deck

let draw_from_deck deck =
  match deck with
  | [] ->
      raise
        (Failure
           "Violates preconditions (cannot draw a card from an empty deck)")
  | [ h ] -> []
  | h :: t -> t

(**[rank_to_string i] onverts the numerical rank [i] of a card to its
   corresponding string. IE: 1 -> "A", 2 -> "2", etc. Requires: i <= 14 and i >=
   2*)
let rank_to_string rank =
  match rank with
  | 14 -> "A"
  | 13 -> "K"
  | 12 -> "Q"
  | 11 -> "J"
  | _ -> Int.to_string rank

let card_to_string card =
  match card.suit with
  | Spades -> "♠"
  | Hearts -> "♥"
  | Diamonds -> "♦"
  | Clubs -> "♣"

let rec cards_to_string (deck : card list) =
  match deck with
  | [] -> ""
  | h :: b :: c :: d :: e :: t ->
      "\n" ^ " ┌─────────┐  ┌─────────┐  ┌─────────┐  ┌─────────┐  ┌─────────┐"
      ^ "\n" ^ " │" ^ rank_to_string h.rank ^ "        │  │"
      ^ rank_to_string b.rank ^ "        │  │" ^ rank_to_string c.rank
      ^ "        │  │" ^ rank_to_string d.rank ^ "        │  │"
      ^ rank_to_string e.rank ^ "        │  " ^ "\n"
      ^ " │         │  │         │  │         │  │         │  │         │ "
      ^ "\n"
      ^ " │         │  │         │  │         │  │         │  │         │ "
      ^ "\n" ^ " │    " ^ card_to_string h ^ "    │  │    " ^ card_to_string b
      ^ "    │  │    " ^ card_to_string c ^ "    │  │    " ^ card_to_string d
      ^ "    │  │    " ^ card_to_string e ^ "    │ " ^ "\n"
      ^ " │         │  │         │  │         │  │         │  │         │ "
      ^ "\n"
      ^ " │         │  │         │  │         │  │         │  │         │ "
      ^ "\n" ^ " │        " ^ rank_to_string h.rank ^ "│  │        "
      ^ rank_to_string b.rank ^ "│  │        " ^ rank_to_string c.rank
      ^ "│  │        " ^ rank_to_string d.rank ^ "│  │        "
      ^ rank_to_string e.rank ^ "│ " ^ "\n"
      ^ " └─────────┘  └─────────┘  └─────────┘  └─────────┘  └─────────┘ "
  | h :: b :: c :: d :: t ->
      "\n" ^ " ┌─────────┐  ┌─────────┐  ┌─────────┐  ┌─────────┐  ┌─────────┐ "
      ^ "\n" ^ " │" ^ rank_to_string h.rank ^ "        │  │"
      ^ rank_to_string b.rank ^ "        │  │" ^ rank_to_string c.rank
      ^ "        │  │" ^ rank_to_string d.rank ^ "        │  │░░░░░░░░░│ "
      ^ "\n"
      ^ " │         │  │         │  │         │  │         │  │░░░░░░░░░│ "
      ^ "\n"
      ^ " │         │  │         │  │         │  │         │  │░░░░░░░░░│ "
      ^ "\n" ^ " │    " ^ card_to_string h ^ "    │  │    " ^ card_to_string b
      ^ "    │  │    " ^ card_to_string c ^ "    │  │    " ^ card_to_string d
      ^ "    │  │░░░░░░░░░│ " ^ "\n"
      ^ " │         │  │         │  │         │  │         │  │░░░░░░░░░│ "
      ^ "\n"
      ^ " │         │  │         │  │         │  │         │  │░░░░░░░░░│ "
      ^ "\n" ^ " │        " ^ rank_to_string h.rank ^ "│  │        "
      ^ rank_to_string b.rank ^ "│  │        " ^ rank_to_string c.rank
      ^ "│  │        " ^ rank_to_string d.rank ^ "│  │░░░░░░░░░│ " ^ "\n"
      ^ " └─────────┘  └─────────┘  └─────────┘  └─────────┘  └─────────┘ "
  | h :: b :: c :: t ->
      "\n" ^ " ┌─────────┐  ┌─────────┐  ┌─────────┐  ┌─────────┐  ┌─────────┐ "
      ^ "\n" ^ " │" ^ rank_to_string h.rank ^ "        │  │"
      ^ rank_to_string b.rank ^ "        │  │" ^ rank_to_string c.rank
      ^ "        │  │░░░░░░░░░│  │░░░░░░░░░│ " ^ "\n"
      ^ " │         │  │         │  │         │  │░░░░░░░░░│  │░░░░░░░░░│ "
      ^ "\n"
      ^ " │         │  │         │  │         │  │░░░░░░░░░│  │░░░░░░░░░│ "
      ^ "\n" ^ " │    " ^ card_to_string h ^ "    │  │    " ^ card_to_string b
      ^ "    │  │    " ^ card_to_string c ^ "    │  │░░░░░░░░░│  │░░░░░░░░░│ "
      ^ "\n"
      ^ " │         │  │         │  │         │  │░░░░░░░░░│  │░░░░░░░░░│ "
      ^ "\n"
      ^ " │         │  │         │  │         │  │░░░░░░░░░│  │░░░░░░░░░│ "
      ^ "\n" ^ " │        " ^ rank_to_string h.rank ^ "│  │        "
      ^ rank_to_string b.rank ^ "│  │        " ^ rank_to_string c.rank
      ^ "│  │░░░░░░░░░│  │░░░░░░░░░│ " ^ "\n"
      ^ " └─────────┘  └─────────┘  └─────────┘  └─────────┘  └─────────┘ "
  | h :: b :: t ->
      "\n" ^ " ┌─────────┐  ┌─────────┐ " ^ "\n" ^ " │" ^ rank_to_string h.rank
      ^ "        │  │" ^ rank_to_string b.rank ^ "        │ " ^ "\n"
      ^ " │         │  │         │ " ^ "\n" ^ " │         │  │         │ "
      ^ "\n" ^ " │    " ^ card_to_string h ^ "    │  │    " ^ card_to_string b
      ^ "    │ " ^ "\n" ^ " │         │  │         │ " ^ "\n"
      ^ " │         │  │         │ " ^ "\n" ^ " │        "
      ^ rank_to_string h.rank ^ "│  │        " ^ rank_to_string b.rank ^ "│ "
      ^ "\n" ^ " └─────────┘  └─────────┘ "
  | h :: t -> ""

let player_to_string player =
  if List.length player.hand > 0 then
    player.name ^ ": XX XX " ^ string_of_int player.balance ^ " Chips"
  else player.name ^ ": __ __ " ^ string_of_int player.balance ^ " Chips"

let revealed_player_to_string player =
  player.name ^ ":"
  ^ cards_to_string player.hand
  ^ "\n          "
  ^ string_of_int player.balance
  ^ " Chips"
