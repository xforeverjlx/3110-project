type bot_level = 
| Easy 
| Medium
| Hard
| None

type player

exception InsufficientFund

val create_player : string -> int -> int -> bool * bot_level -> player
(** [create_player name wealth position] returns a player with name as
    [name], wealth as [wealth]. *)

val create_player_full : string -> int -> Card.t -> int -> int -> bool * bot_level ->player
(** [create_player_full name wealth cards amount_placed_on_table position]
    returns a player with name as [name], wealth as [wealth], cards as
    [cards], amount_placed_on_table as [amount_placed_on_table]. *)

val name : player -> string
val wealth : player -> int
val is_bot: player -> (bool * bot_level)
val amount_placed : player -> int
val cards : player -> Card.t
val position : player -> int

val set_cards : player -> Card.t -> player
(** [set_cards player cards] returns [player] possessed with [cards]. *)

val reset_player : player -> player
(** [reset_player] removes all cards and set amount on table as 0. *)

val set_wealth : player -> int -> player
(** [set_wealth player] returns [player] with [amount] of wealth *)

val deduct : player -> int -> player
(** [deduct player amount] returns [player] with [amount] deducted from
    their wealth. Raises [InsufficientFund] if player's wealth is less
    than [amount].*)

val add : player -> int -> player
(** [add player amount] returns [player] with [amount] added to their
    wealth. *)
