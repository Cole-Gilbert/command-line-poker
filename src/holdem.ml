(*Module representing static holdem data.*)

(**Enumeration type for suit of a card.*)
type suit =
  | Spades
  | Hearts
  | Clubs
  | Diamonds

type card = {
  suit : suit;
  rank : int;
}

type player = {
  name : string;
  balance : int;
  betting : int;
  active : bool;
  card1 : card;
  card2 : card;
}

(**[fresh_deck] is a set-like list of all 52 playing cards in order. IE: Spades
   (A -> K), Hearts (A -> K), Diamonds (A -> K), Clubs (A -> K).*)
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

(**[shuffle deck] is a set-like list that randomizes the locations of the cards
   in the original deck [d]. Requires: [deck] has size 52.*)
let shuffle deck = raise (Failure "Unimplemented.")

let shuffled_deck = raise (Failure "Unimplemented.")
let top_card deck = raise (Failure "Unimplemented.")
let draw_from_deck deck = raise (Failure "Unimplemented.")
let card_to_string card = raise (Failure "Unimplemented.")
let deck_to_string deck = raise (Failure "Unimplemented.")