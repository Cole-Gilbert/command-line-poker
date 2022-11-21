(** Functions for comparing poker hands

    This module contains all the functionality necessary to handle showdowns in
    Texas Hold'em Poker *)

val showdown : Holdem.card list -> Holdem.player list -> Holdem.player list
(** [showdown board players] is the player(s) in [players] with the winning 5
    card poker hand made from a combination of their 2 personal cards and the
    board cards from [board]. Can be multiple players in the case of a chop.
    Requires: board to be of length 5 and player hands to be of length 2 *)
