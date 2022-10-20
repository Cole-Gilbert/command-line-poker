type turn =
  | Deal
  | Call
  | Raise of int
  | Check
  | Fold

type edit =
  | AddPlayer of string
  | RemovePlayer of string

type command =
  | Turn of turn
  | Edit of edit
  | Quit

let parse str = Quit
