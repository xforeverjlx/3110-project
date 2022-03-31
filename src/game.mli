open Player

type game = {
  active_players : player list;
  fold_collection : player list;
  current_deck : Card.t;
  cards_on_table : Card.t;
  pot : int;
  small_blind : player;
  small_blind_amt : int;
  current_bet : int;
  minimum_raise : int;
  consecutive_calls : int;
  new_round : bool;
  winners : player list;
  position : int;
}
(** The abstract type of values representing game. *)

exception RaiseFailure (* raise is less than previous raise. *)

exception PlayerNotFound
exception DuplicateName
exception NotEnoughPlayers

type command =
  | Call
  | Raise of int
  | Fold

val create_game : player list -> int -> game
(** [create_game] initialize the game with player list passed in from
    the interface player, deal hands for each player. First player is
    small blind. It also automatically move for small and big blind, so
    the next player to move is the one after big blind. *)

val play_again : game -> game
(** [play again] reinitialize the game with the same players from the
    previous game but shift the small blind to the next person. Other
    parts are the same as create_game. Precondition: reshuffling period
    (active_players is sorted by position) *)

val reshuffling_period : game -> game
(** [reshuffling_period] moves all players to active_players and sort.
    Prepares for adding funds, adding new players, and removing players.
    Precondition: game is over *)

val add_fund : game -> string -> int -> game
(** [add_fund game name amount] adds amount to wealth of player with
    name str. Precondition: reshuffling period. Raises: PlayerNotFound,
    Failure is amount is negative *)

val add_player : game -> string -> int -> game
(** [add_player game name wealth] adds player with name and wealth to
    players in game. Precondition: reshuffling period. Raise:
    DuplicateName, Failure if wealth is negative. *)

val remove_player : game -> string -> game
(** [remove_player g str] removes player with name str from the game.
    recondition: reshuffling period. Raises: PlayerNotFound,
    NotEnoughPlayers*)

val execute_command : game -> command -> game * int
(** [betting_round g] is the game state after executing the player's
    next move, and the amount of money the player added to the pot *)

val get_curr_player : game -> player
(** [get_curr_player game] returns the player who is making the decision
    of pass/raise/fold. Read only. *)

val get_winners : game -> player list
(** [get_winner game] returns the players who won. Precondition: game
    had ended. Read only. *)

val get_winning_hand : game -> string
(** [get_winning_hand] returns the rank of winninng hand. Precondition:
    game has ended. Read only. *)

val get_all_players : game -> player list
(** [get_all_players game] returns all the players in the game, sorted
    by their positions. Read only. *)

val get_legal_moves : game -> string list
(** [get_available_moves game] returns the legal moves a player can make
    this turn. Read only. *)

val save_game : game -> string -> bool
(** [save_game game name] saves the current game to a [name].json,
    returns true if successful, false otherwise. *)

exception BadFormat

val read_game : Yojson.Basic.t -> game
(** [read_game j] returns the game stored in [j]. Raises [BadFormat] if
    [j] is badly formatted. *)
