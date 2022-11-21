(** Representation of static holdem data.

    This module represents the data stored in the Texas Hold 'em poker game
    files, including the players and the deck of cards. *)

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
(**The abstract type of values representing a playing card.*)

type player = {
  name : string;
  balance : int;
  betting : int;
  active : bool;
  hand : card list;
}
(**The abstract type of values representing a poker player.*)

val make_player : string -> int -> player
(**[make_player name balance hand] initializes a player with name: [name];
   balance: [balance]; betting: 0; active: True; and hand:[].*)

val deal_to : card -> player -> player
(**[deal_to c p] adds a card [c] to a player's [p] hand. Requires: player's [p]
   hand must have size < 2. *)

val pay_amount : int -> player -> player
(**[pay_amount i p] removes chips [i] from a player's [p] balance. *)

val fresh_deck : card list
(**[fresh_deck] is a list (without duplicates) of all 52 playing cards in order.
   IE: Spades (A -> K), Hearts (A -> K), Diamonds (A -> K), Clubs (A -> K).*)

val shuffled_deck : unit -> card list
(**[shuffled_deck] is a list (without duplicates) of a randomized deck of 52
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

val cards_to_string : card list -> string
(**[cards_to_string lst] is the string representation of a card list [lst]. *)

val player_to_string : player -> string
(**[player_to_string p] is the string representation of a player [p] without
   revealing their hand.*)

val revealed_player_to_string : player -> string
(**[revealed_player_to_string p] is the string representation of a player [p]
   showing the cards in their hand.*)
