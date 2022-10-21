(** Representation of a use input command

    This module represents the possible commands a player can enter. It handles
    parsing a string into the data types specified *)

type action =
  | Deal
  | Call
  | Raise of int
  | Check
  | Fold
  | AddPlayer of string
  | RemovePlayer of string  (** The type representing a turn made by a player *)

type command =
  | Action of action
  | Quit  (** The type representing a whole player command *)

exception Empty
(** Raised when attempting to parse an empty command *)

exception Malformed
(** Raised when attempting to parse an invalid command *)

val parse : string -> command
(** [parse str] is the parsed command of the user input given as a string *)