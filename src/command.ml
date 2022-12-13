type action =
  | Comfirm
  | Deal
  | Call
  | Check
  | Fold
  | Raise of int
  | AddPlayer of string
  | RemovePlayer of string

exception Malformed

type command =
  | Action of action
  | Quit
  | Help

let parse str =
  match String.split_on_char ' ' str |> List.filter (fun s -> s <> "") with
  | [] -> Action Comfirm
  | [ "help" ] -> Help
  | [ "quit" ] -> Quit
  | [ "deal" ] -> Action Deal
  | [ "call" ] -> Action Call
  | [ "check" ] -> Action Check
  | [ "fold" ] -> Action Fold
  | [ "raise"; i ] -> (
      try Action (Raise (int_of_string i)) with Failure _ -> raise Malformed)
  | "add" :: t when t != [] -> Action (AddPlayer (String.concat " " t))
  | "remove" :: t when t != [] -> Action (RemovePlayer (String.concat " " t))
  | _ -> raise Malformed
