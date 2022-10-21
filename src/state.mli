(** Representation of dynamic Holdem state.

    This module represents the state of an Holdem game as it is being played,
    including the players, deck, pot, community cards, and functions that cause
    the state to change. *)

type t
(** The abstract type of values representing the game state. *)

(** The type representing the result of an attempted turn. *)
type result =
  | Legal of t
  | Illegal of string

val init : int -> t
(** [init i] is the initial state of the game when playing holdem with i
    representing the buy in amount. In that state the holdem game has no players
    but can be added before the start of each round*)

val action : Command.action -> t -> result
(** [action c a] applies the action based on command c which contains necessary
    information to edit a, which represents the game state.*)

val quit : t -> string
(** [quit a] returns a string representing the winner(s) of the game state,
    given by a, by the number of chips of each player*)

val state_to_string : t -> string
(** [state_to_string a] returns the string representation of a, which is the
    state of the game *)
