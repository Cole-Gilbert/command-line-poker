(*Module representing static holdem data.*)

(**Enumeration type for suit of a card.*)
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

let fresh_deck =
  [
    (*Spades A -> K*)
    { suit = Spades; rank = 1 };
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
    { suit = Hearts; rank = 1 };
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
    { suit = Diamonds; rank = 1 };
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
    { suit = Clubs; rank = 1 };
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

(**[shuffle deck] is a list (without duplicates) that randomizes the locations
   of the cards in the original deck [d]. Requires: [deck] has size 52.*)
let shuffle deck = raise (Failure "Unimplemented.")

let shuffled_deck = shuffle fresh_deck
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
   corresponding string. IE: 1 -> "A", 2 -> "2", etc. Requires: i <= 13 and i >=
   1*)
let rank_to_string rank =
  match rank with
  | 13 -> "K"
  | 12 -> "Q"
  | 11 -> "J"
  | 1 -> "A"
  | _ -> Int.to_string rank

let card_to_string card =
  match card.suit with
  | Spades -> rank_to_string card.rank ^ "S"
  | Hearts -> rank_to_string card.rank ^ "H"
  | Diamonds -> rank_to_string card.rank ^ "D"
  | Clubs -> rank_to_string card.rank ^ "C"

let deck_to_string deck =
  List.fold_left (fun acc card -> acc ^ card_to_string card ^ " ") " " deck
