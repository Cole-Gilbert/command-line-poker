(* Parses the user input input *)

(* Data type for a command *)
type command =
  | Deal
  | AddPlayer of (String * Int) (* The name and their initial coins *)
  | Leave

(* Parse function String -> command *)
(* E.g. "Deal" makes Deal
   "AddPlayer Sean 100" makes AddPlayer holding tuple ("Sean", 100)
   "Leave" makes leave *)