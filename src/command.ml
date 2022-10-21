type turn =
  | Deal
  | Call
  | Raise of int
  | Check
  | Fold

exception Empty
exception Malformed

type edit =
  | AddPlayer of string
  | RemovePlayer of string

type command =
  | Turn of turn
  | Edit of edit
  | Quit

let parse str =
  match String.split_on_char ' ' str |> List.filter (fun s -> s <> "") with
  | [] -> raise Empty
  | [ h ] ->
      if String.equal h "quit" then Quit
      else if String.equal h "deal" then Turn Deal
      else raise Malformed
  | h :: t ->
      if String.equal h "add" then Edit (AddPlayer (String.concat " " t))
      else if String.equal h "remove" then
        Edit (RemovePlayer (String.concat " " t))
      else raise Malformed
