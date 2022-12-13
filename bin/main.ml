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
  match Command.parse (read_line ()) with
  | exception End_of_file -> Quit
  | exception Command.Malformed ->
      ANSITerminal.erase Above;
      print_string
        "Malformed Command. Please try again. Type \"help\" for the list of \
         commands\n";
      get_input st
  | cmd ->
      ANSITerminal.erase Above;
      cmd

(** [game_loop st] is the new state after following a command, or the current
    state again if the command was Illegal *)
let rec game_loop st =
  match get_input st with
  | Action cmd -> State.action cmd st |> validate st |> game_loop
  | Quit -> State.quit st |> print_string
  | Help ->
      print_string
        {|Game Commands:
  "help" prints these instructions and can be used at any point  
  "quit" quits the game and announces the winner(s)
  "add [Player Name]" adds a player to the game with the provided name
  "remove [Player Name]" removes a player with that exact name if they exist
  "deal" starts a game with the current players. Must have at least 2 players
  "[Enter]" is used to confirm your turn and show your cards

Texas Holdem Commands:
  "check" passes the action to the next player. Can be used when no bets have yet been made
  "call" matches the current minimum bet
  "fold" forfeits the money you have bet and removes you from the round
  "raise [Amount]" raises the bet by [Amount]. [Amount] must be greater than the big blind (1/100 of buy-in) and re-raises must at least double the pot

|};
      game_loop st

(** [buyin ()] is a valid integer buy-in amount *)
let rec buyin () =
  match read_line () |> String.trim |> int_of_string with
  | exception _ ->
      print_string "Please enter an integer.\n> ";
      buyin ()
  | i ->
      if i <= 0 then (
        print_string "Please enter a positive integer.\n> ";
        buyin ())
      else i

let main () =
  ANSITerminal.print_string []
    "\n\nWelcome to Texas Hold'em! What is your buy-in?\n> ";
  State.init (buyin ()) |> game_loop

(* Entry Point *)
let () = main ()