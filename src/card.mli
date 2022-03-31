type card =
  | S of int
  | H of int
  | C of int
  | D of int
      (** The suits are [S]pades, [H]earts, [C]lubs [D]iamonds.
          Examples:

          - [S 13] represents King of Spades
          - [D 1] represents Ace of Diamonds
          - [H 7] represents 7 of Hearts *)

type t = card list
(** Represents one deck of cards. *)

val to_string : t -> string
(** [to_string hand] returns a string representation of [hand]. *)

val pretty_print : t -> string
(** [pretty_print hand] returns a pretty string representation of
    [hand]. *)

val new_deck : t
(** [new_deck] returns a new deck of poker cards. There are 52 cards. *)

val n_random_card : t -> int -> t * t
(** [n_random_card deck amount] returns [(cards, new_deck)] where
    [cards] is a list of distinct random cards from [deck] and
    [new_deck] is [deck] with elements of [cards] removed.

    Requires: [amount] >= 0 && [amount] <= (List.length [deck]). *)

val rank_of_hand : t -> int
(** [rank_of_hand hand] returns the rank of [hand] according to texas
    holdem card ranking rules as specified in
    docs/card_ranking_rules.txt.

    Requires: 5 <= [List.length hand] and [List.length hand] <= 7. *)

val starting_hand_estimated_strength : t -> int
(** [starting_hand_estimated_strength hand] returns the estimated
    strength of [hand], which is an integer between 0 and 5, where 5 is
    the strongest and 0 the weakest.

    Requires: [List.length hand] = 2. *)

exception Tie of int list

val index_of_highest_hand : t list -> int
(** [index_of_highest_hand card_list_list] returns the index of the
    highest hand in [card_list_list], using texas holdem card ranking
    rules as specified in docs/card_ranking_rules.txt.

    Requires: for any element e in [card_list_list], [List.length e] =
    7.

    Raises: [Tie] of \[i1 ... in \] when hands at index i1 ... in in
    [card_list_list] are tied. *)

(********************************************************************
    Below are mainly for testing purposes.
 ********************************************************************)

val equal : card -> card -> bool
(** [equal c1 c2] returns true if suit of c1 equals suit of c2 and
    number of c1 equals number of c2, false otherwise. *)

val single_compare : card -> card -> int
(** [compare card1 card2] returns 1 if [card1] is larger than [card2]
    (using poker comparison rules), -1 if [card1] is less than [card2],
    0 if [card1] = [card2]. Examples:

    - [compare (S 3) (S 5)] is [-1]
    - [compare (C 1) (H 12)] is [1] *)

val has_pair : t -> bool
(** [has_pair hand] returns true if there is at least one pair in
    [hand], false otherwise. *)

val has_two_pair : t -> bool
(** [has_two_pair hand] returns true if there is at least two distinct
    pairs in [hand], false otherwise. Examples:

    - [has_two_pair \[C 5; D 5; S 4; H 4; S 2; S 1; H 1\]] is true
    - [has_two_pair \[C 5; D 5; S 5; H 5; S 2; S 1; H 1\]] is true
    - [has_two_pair \[C 5; D 5; S 5; H 5; S 2; S 1; H 0\]] is false *)

val has_three_of_a_kind : t -> bool
(** [has_three_of_a_kind hand] returns true if there is at least three
    cards of the same value in [hand], false otherwise. *)

val has_straight : t -> bool
(** [has_straight hand] returns true if [hand] has at least five
    consecutive cards (using poker comparison rules), false otherwise. *)

val has_flush : t -> bool
(** [has_flush hand] returns true if [hand] has at least five cards of
    the same suit, false otherwise. *)

val has_full_house : t -> bool
(** [has_full_house hand] returns true if [hand] has at least three
    cards of one value and two cards of another, false otherwise. *)

val has_four_of_a_kind : t -> bool
(** [has_four_of_a_kind hand] returns true if there is at least four
    cards of the same value in [hand], false otherwise. *)

val has_straight_flush : t -> bool
(** [has_straight_flush hand] returns true if [hand] has at least five
    consecutive cards of the same suit, false otherwise. *)

val has_royal_flush : t -> bool
(** [has_royal_flush hand] returns true if [hand] has 'A', 'K', 'Q',
    'J', 10 of the same suit, false otherwise. *)