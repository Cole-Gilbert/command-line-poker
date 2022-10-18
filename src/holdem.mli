(** Representation of static holdem data.

    This module represents the data stored in the Texas Hold 'em poker game
    files, including the players and the deck of cards. *)

type card
(**The abstract type of values representing a playing card.*)

type player
(**The abstract type of values representing a poker player.*)

val shuffled_deck : card list -> card list
(**[shuffled_deck] is a set-like list of a randomized deck of 52 cards.*)

val top_card : card list -> card
(**[top_card d] is the top card from a deck [d]*)

val draw_from_deck : card list -> card list
(**[draw_from_deck d] is the resulting set-like list of cards after removing the
   top card from deck [d]. Requires: [d] is not empty. *)

val card_to_string : card -> string
(**[card_to_string c] is the string representation of a card [c]. *)

val deck_to_string : card list -> string
(**[deck_to_string d] is the string representation of a deck [d]. *)
