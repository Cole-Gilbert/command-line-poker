(** Representation of a use input command

    This module represents the possible commands a player can enter. It handles
    parsing a string into the data types specified *)

type action =
  | Comfirm
  | Deal
  | Call
  | Check
  | Fold
  | Raise of int
  | AddPlayer of string
  | RemovePlayer of string  (** The type representing a turn made by a player *)

type command =
  | Action of action
  | Quit  (** The type representing a whole player command *)

exception Malformed
(** Raised when attempting to parse an invalid command *)

val parse : string -> command
(** [parse str] is the parsed command of the user input given as a string *)