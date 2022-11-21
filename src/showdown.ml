(** Functions for comparing poker hands *)

(* Type for storing kicker cards for tie ranks *)
type kickers = int list

(* Type for representing a hand and its strength *)
type hand =
  | RoyalFlush of int
  | StraightFlush of int
  | FourOfAKind of int
  | FullHouse of (int * int)
  | Flush of int
  | Straight of int
  | ThreeOfAKind of (int * kickers)
  | TwoPair of (int * int * kickers)
  | Pair of (int * kickers)
  | HighCard of (int * kickers)

(* Type for indicating winner or ties *)
type result =
  | Win
  | Loss
  | Chop

(*****************************************************************************
  Auxillary functions for rank_5 make use of either an (int * int) tuple list
  representing the rank and multiplicity of a card in a hand, or a basic
  Holdem.card list representing the hand.*)

(*Auxillary functions named "find_..." take in an (int * int) list while 
   functions named "check_..." take in a Holdem.card list
 *****************************************************************************)

(** [is_flush cards] is true if all the cards in [cards] have the same suit 
    Requires: [cards] is not empty.*)
let is_flush (cards : Holdem.card list) : bool = 
  match cards with
  | h::t -> 
    (let rec is_suited (suit : Holdem.suit) (cards : Holdem.card list) = 
    match cards with
    | h::t -> if h.suit = suit then is_suited suit t else false
    | [] -> true in is_suited h.suit t)
  | [] -> failwith "Violates precondition (Hand cannot be empty)"

(** [is_straight cards] is true if all the cards in [cards] are in order 
  IE 2,3,4,5,6 
  Requires: [cards] is not empty, [cards] contains no aces.*)
let is_straight (cards : Holdem.card list) : bool = 
  let rec is_rank_below (prev : int) (cards : Holdem.card list) : bool =
  match cards with
  | h::t -> if prev = h.rank then is_rank_below h.rank t else false
  | [] -> true
  in is_rank_below (List.hd cards).rank (List.tl cards)

(**[remove_ace cards] is the card list of cards in [cards] with any aces removed
    . Requires: [cards] in reverse sorted order (decreasing, by rank),
      [cards] is not empty.*)
let remove_ace (cards : Holdem.card list) : Holdem.card list = 
    match cards with 
    | h::t -> if h.rank = 14 then t else cards
    | [] -> failwith "Violates precondition (Hand cannot be empty)"

(** [to_rank_mult cards] is a helper method that transforms [cards] to a list of
    (int * int) tuples containing the respective ranks and multiplicities of the
    cards in [cards]*)
let to_rank_mult (cards : Holdem.card list) =
  let rec to_rank_mult_aux (hand : Holdem.card list) : (int * int) list =
    match hand with
    | h :: t ->
        let filtered_list =
          List.filter (fun (elt : Holdem.card) -> elt.rank <> h.rank) t
        in
        (h.rank, List.length hand - List.length filtered_list)
        :: to_rank_mult_aux filtered_list
    | [] -> []
  in
  to_rank_mult_aux cards

(** [find_rank_of_mult mult rank_mult_list] is Some int or None if there is no
    multiplicity of [mult] in [rank_mult_list]*)
let find_rank_of_mult mult (rm_list : (int * int) list) : int option =
  let rec frm_aux mult (rm_list : (int * int) list) : int option =
    match rm_list with
    | (rank, multiplicity) :: t ->
        if multiplicity = mult then Some rank else frm_aux mult t
    | [] -> None
  in
  frm_aux mult rm_list
(**[remove_by_rank rank rank_mult_list] removes the element with rank [rank] 
    from [rank_mult_list]. List remains unchanged if [rank_mult_list] does not 
      contain an element with rank [rank]*)
let remove_by_rank rank (rm_list : (int * int) list) =
  List.filter (fun elt -> fst elt <> rank) rm_list

(** [to_kickers rank_mult_list] is a int list of all the card ranks in
    [rank_mult_list] Requires: multiplicities in [rank_mult_list] must be 1*)
let to_kickers (rm_list : (int * int) list) : int list =
  let rec to_kickers_aux (rm_list : (int * int) list) =
    match rm_list with
    | (rank, _) :: t -> rank :: to_kickers_aux t
    | [] -> []
  in
  to_kickers_aux rm_list

(** [check_highcard cards] converts [cards] to [(Highcard (rank * kickers)]*)
let check_highcard (cards : Holdem.card list) : hand =
  match cards with
  | h :: t ->
      HighCard
        ( h.rank,
          List.fold_left
            (fun kickers (elt : Holdem.card) -> elt.rank :: kickers)
            [] t )
  | [] -> failwith "Violates precondition (Hand cannot be empty)"

(**[check_seq cards] is [Some hand] or None if the cards do not contain a
   straight, flush, straight flush, or royal straight. *)
let check_seq (cards : Holdem.card list) : hand option =
  if cards |> is_flush then 
    let no_ace_hand = remove_ace cards in
    if no_ace_hand |> is_straight then
      if (List.length no_ace_hand = 4) then
        if ((List.hd no_ace_hand).rank = 13) then
          Some (RoyalFlush (14))
        else Some (StraightFlush ((List.hd cards).rank))
      else Some (StraightFlush ((List.hd cards).rank))
    else Some (Flush ((List.hd cards).rank))
  else
    if cards |> is_straight then 
      Some (Straight ((List.hd cards).rank))
    else
      None

(**[find_pair rank_mult_list] is Some [Pair (rank * kickers)] or None if
   [rank_mult_list] doesn't contain a pair.*)
let find_pair (rm_list : (int * int) list) : hand option =
  let pair_rank = find_rank_of_mult 2 rm_list in
  match pair_rank with
  | Some rank -> Some 
    (Pair (rank, ((remove_by_rank rank rm_list) |> to_kickers)))
  | None -> None

(**[find_two_pair rank_mult_list] is Some [TwoPair (rank * kickers)] or None if
   [rank_mult_list] doesn't contain a two pair.*)
let find_two_pair (rm_list : (int * int) list) : hand option =
  let first_pair_rank = find_rank_of_mult 2 rm_list in
  match first_pair_rank with
  | Some first_rank -> 
    (let second_pair_rank = 
      find_rank_of_mult 2 (remove_by_rank first_rank rm_list) in 
    match second_pair_rank with
    | Some second_rank -> 
      Some (TwoPair (first_rank, second_rank, 
      ((remove_by_rank second_rank rm_list) |> to_kickers)))
    | None -> None)
  | None -> None

(**[find_three_of_a_kind rank_mult_list] is Some [ThreeOfAKind (rank * kickers)]
   or None if [rank_mult_list] doesn't contain a three of a kind.*)
let find_three_of_a_kind (rm_list : (int * int) list) : hand option =
  let three_oak_rank = find_rank_of_mult 3 rm_list in
  match three_oak_rank with
  | Some rank -> 
    Some (Pair (rank, ((remove_by_rank rank rm_list) |> to_kickers)))
  | None -> None

(**[find_full_house rank_mult_list] is Some [FullHouse (rank * rank)] or None if
   [rank_mult_list] doesn't contain a full house.*)
let find_full_house (rm_list : (int * int) list) : hand option =
  let three_oak_rank = find_rank_of_mult 3 rm_list in
  match three_oak_rank with
  | Some three_rank -> 
    (let pair_rank = find_rank_of_mult 2 rm_list in
    match pair_rank with
    | Some two_rank -> Some (FullHouse (three_rank, two_rank))
    | None -> None)
  | None -> None

(**[find_four_of_a_kind rank_mult_list] is Some [FourOfAKind (rank)] or None if
   [rank_mult_list] doesn't contain a four of a kind.*)
let find_four_of_a_kind (rm_list : (int * int) list) : hand option =
  let four_oak_rank = find_rank_of_mult 2 rm_list in
  match four_oak_rank with
  | Some rank -> Some (FourOfAKind (rank))
  | None -> None

(** [rank_5 cards] is the hand representation of a five card poker hand *)
let rank_5 (cards : Holdem.card list) : hand =
  let rank_mult_list = cards |> to_rank_mult in
  let rml_size = List.length rank_mult_list in

  if rml_size = 5 then
    (*Each card has unique rank*)
    match check_seq cards with
    | Some hand -> hand
    | None -> check_highcard cards
  else if rml_size = 4 then
    match find_pair rank_mult_list with
    (*One card is a pair*)
    | Some hand -> hand
    | None -> failwith "Impossible state"
  else if rml_size = 3 then
    (*Either three of a kind or two pair.*)
    match find_two_pair rank_mult_list with
    | Some hand -> hand
    | None -> (
        match find_three_of_a_kind rank_mult_list with
        | Some hand -> hand
        | None -> failwith "Impossible state")
  else if rml_size = 2 then
    (*Either full house or four of a kind*)
    match find_full_house rank_mult_list with
    | Some hand -> hand
    | None -> (
        match find_four_of_a_kind rank_mult_list with
        | Some hand -> hand
        | None -> failwith "Impossible state")
  else failwith "Impossible state" (*Impossible with normal 52 card deck*)

(** [hand_value h] is the int list representing the strength of the hand.
    Greater strength is indicated by a higher int, and each int takes precedent
    over all ints behind it in the list (which would be used in case of a tie in
    strength earlier in the list). *)
let hand_value (h : hand) : int list =
  match h with
  | RoyalFlush rank -> [ 9; rank ]
  | StraightFlush rank -> [ 8; rank ]
  | FourOfAKind rank -> [ 7; rank ]
  | FullHouse (rank1, rank2) -> [ 6; rank1; rank2 ]
  | Flush rank -> [ 5; rank ]
  | Straight rank -> [ 4; rank ]
  | ThreeOfAKind (rank, kickers) -> 3 :: rank :: kickers
  | TwoPair (rank1, rank2, kickers) -> 2 :: rank1 :: rank2 :: kickers
  | Pair (rank, kickers) -> 1 :: rank :: kickers
  | HighCard (rank, kickers) -> 0 :: rank :: kickers

(** [compare_hands h1 h2] is the result [Win] if the first hand is stronger,
    [Loss] if the second hand is stronger, and [Chop] if they are even *)
let compare_hands (h1 : hand) (h2 : hand) : result =
  let rec compare_hands_aux (hv1 : int list) (hv2 : int list) : result =
    match (hv1, hv2) with
    | [], [] -> Chop
    | r1 :: t1, r2 :: t2 ->
        if r1 > r2 then Win
        else if r2 > r1 then Loss
        else compare_hands_aux t1 t2
    | _ -> Chop (* Defensive *)
  in
  compare_hands_aux (hand_value h1) (hand_value h2)

(** [sort_cards_dec cards] is the decreasing sorted (by rank) list of cards in
    [cards] *)
let sort_cards_dec (cards : Holdem.card list) : Holdem.card list =
  List.sort Holdem.compare cards |> List.rev

(** [card_combos lst] is a list of lists, one for each unique 5 item combination
    of [lst] *)
let rec card_combos lst =
  let rec card_combos_aux acc len lst =
    if len = 0 then [ [] ]
    else
      match lst with
      | [] -> acc
      | h :: t ->
          let rec combo_map acc f = function
            | [] -> acc
            | h :: t -> combo_map (f h :: acc) f t
          in
          let gen =
            combo_map acc (fun z -> h :: z) (card_combos_aux [] (len - 1) t)
          in
          card_combos_aux gen len t
  in
  card_combos_aux [] 5 lst

(** [best_hand board player] is the hand representation of the player's best
    possible 5 card hand out of the 5 board cards and their two personal cards.
    Requires: board to be of length 5 and player hand to be of length 2 *)
let best_hand (board : Holdem.card list) (player : Holdem.player) : hand =
  let sorted = sort_cards_dec (player.hand @ board) in
  let hands = List.map rank_5 (card_combos sorted) in
  match hands with
  | [] -> failwith "Invalid"
  | h :: t ->
      List.fold_left
        (fun h1 h2 ->
          if compare_hands h1 h2 = Win || compare_hands h1 h2 = Chop then h1
          else h2)
        h t

let showdown (board : Holdem.card list) (players : Holdem.player list) :
    Holdem.player list =
  let fold_aux (win_acc : Holdem.player list * hand option) (p : Holdem.player)
      =
    let p_hand = best_hand board p in
    match win_acc with
    | [], None | [], Some _ | _, None -> ([ p ], Some p_hand)
    | winners, Some top_hand -> (
        match compare_hands p_hand top_hand with
        | Win -> ([ p ], Some p_hand)
        | Loss -> win_acc
        | Chop -> (p :: winners, Some top_hand))
  in
  let winners, _ = List.fold_left fold_aux ([], None) players in
  winners
