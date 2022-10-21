type action =
  | Deal
  | Call
  | Raise of int
  | Check
  | Fold
  | AddPlayer of string
  | RemovePlayer of string

exception Empty
exception Malformed

type command =
  | Action of action
  | Quit

let parse str =
  match String.split_on_char ' ' str |> List.filter (fun s -> s <> "") with
  | [] -> raise Empty
  | [ h ] ->
      if String.equal h "quit" then Quit
      else if String.equal h "deal" then Action Deal
      else raise Malformed
  | h :: t ->
      if String.equal h "add" then Action (AddPlayer (String.concat " " t))
      else if String.equal h "remove" then
        Action (RemovePlayer (String.concat " " t))
      else raise Malformed
