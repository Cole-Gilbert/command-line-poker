open Game
open Command
open State

(** [validate st1 st2] is the new state in [st2] if Legal or the old state [st1]
    if [st2] is Illegal *)
let validate st1 st2 =
  match st2 with
  | Illegal str ->
      print_string str;
      st1
  | Legal st2 -> st2

(** [get_input st] is the parsed command from user input *)
let rec get_input st =
  State.state_to_string st |> print_string;
  print_string "\n> ";
  match read_line () with
  | exception End_of_file ->
      print_string "Please Enter a Command\n";
      get_input st
  | input -> (
      match Command.parse input with
      | exception Command.Malformed ->
          print_string "Malformed Command. Please try again.\n";
          get_input st
      | exception Command.Empty ->
          print_string "Please Enter a Command\n";
          get_input st
      | cmd -> cmd)

(** [game_loop st] is the new state after following a command, or the current
    state again if the command was Illegal *)
let rec game_loop st =
  match get_input st with
  | Action cmd -> State.action cmd st |> validate st |> game_loop
  | Quit -> State.quit st |> print_string

(** [buyin ()] is a valid integer buy-in amount *)
let rec buyin () =
  match read_line () with
  | exception End_of_file ->
      print_string "Please enter an integer.\n> ";
      buyin ()
  | input -> (
      match int_of_string (String.trim input) with
      | exception Failure m ->
          print_string "Please enter an integer.\n> ";
          buyin ()
      | i ->
          if i <= 0 then (
            print_string "Please enter a positive integer.\n> ";
            buyin ())
          else i)

let main () =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\n\nWelcome to Texas Hold'em! What is your buy-in?\n> ";
  State.init (buyin ()) |> game_loop

(* Entry Point *)
let () = main ()