type player = {
  name : string;
  wealth : int;
  cards : Card.t;
  amount_placed_on_table : int;
  position : int;
}

exception InsufficientFund

let create_player
    (input_name : string)
    (input_wealth : int)
    (position : int) =
  {
    name = input_name;
    wealth = input_wealth;
    cards = Card.new_deck;
    amount_placed_on_table = 0;
    position;
  }

let create_player_full
    (input_name : string)
    (input_wealth : int)
    (input_cards : Card.t)
    (input_amount_placed_on_table : int)
    (position : int) =
  {
    name = input_name;
    wealth = input_wealth;
    cards = input_cards;
    amount_placed_on_table = input_amount_placed_on_table;
    position;
  }

let name player = player.name
let wealth player = player.wealth
let amount_placed player = player.amount_placed_on_table
let cards player = player.cards
let position player = player.position
let set_cards player cards = { player with cards }
let set_wealth player amount = { player with wealth = amount }

let reset_player player =
  { player with cards = []; amount_placed_on_table = 0 }

let deduct player amount =
  if player.wealth < amount then raise InsufficientFund
  else
    {
      player with
      wealth = player.wealth - amount;
      amount_placed_on_table = player.amount_placed_on_table + amount;
    }

let add player amount = { player with wealth = player.wealth + amount }