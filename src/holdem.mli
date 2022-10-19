(** Representation of static holdem data.

    This module represents the data stored in the Texas Hold 'em poker game
    files, including the players and the deck of cards. *)

type card
(**The abstract type of values representing a playing card.*)

type player = {
  name : string;
  balance : int;
  betting : int;
  active : bool;
  hand : card list;
}
(**The abstract type of values representing a poker player.*)

val make_player : string -> int -> card list -> player
(**[make_player name balance hand] initializes a player with name: [name];
   balance: [balance]; betting: 0; active: True; and hand:[hand].*)

val fresh_deck : card list
(**[fresh_deck] is a list (without duplicates) of all 52 playing cards in order.
   IE: Spades (A -> K), Hearts (A -> K), Diamonds (A -> K), Clubs (A -> K).*)

val shuffle_deck : unit -> unit
(**[shuffle_deck ()] generates a new randomization of current_deck.*)

val current_deck : card list
(**[current_deck] is a list (without duplicates) of a randomized deck of 52
   cards.*)

val compare : card -> card -> int
(**[compare c1 c2] is positive if card [c1] is greater than card [c2], 0 if card
   [c1] and card [c2] are equal, and negative if card [c1] is less than card
   [c2]*)

val top_card : card list -> card
(**[top_card d] is the top card from a deck [d]*)

val draw_from_deck : card list -> card list
(**[draw_from_deck d] is the resulting list (without duplicates) of cards after
   removing the top card from deck [d]. Requires: [d] is not empty. *)

val card_to_string : card -> string
(**[card_to_string c] is the string representation of a card [c]. *)

val deck_to_string : card list -> string
(**[deck_to_string d] is the string representation of a deck [d]. *)
