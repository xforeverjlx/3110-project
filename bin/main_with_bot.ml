open Texas_holdem
open Game_with_bot
open Player_with_bot
open Card
open Bot

exception Exit of int
(** 0: exit; 1: save game; 2: load file error *)

(** [legal_move] prints the legal moves of the game and 
and the player to choose an action *)
let legal_move game : unit =
  print_endline
    ("\nLegal moves: "
    ^ (get_legal_moves game |> String.concat ", ")
    ^ "\nEnter your move: ")

(** [get_bot_level i ls] gets level of bot and initial wealth from 
user input. If illegal bot level is entered, then no bot is created. 
If no valid wealth entered, then bot starts with initial wealth of 100. 
Then create the bot player and add it to the player list ls*)
let rec get_bot_level i ls =
  print_endline "\nWhich level of PokerBot do you want against?";
  print_endline "Possible mode are: Easy, Medium, Hard";
  print_string "> ";
  try
    let level =
      match read_line () |> String.trim |> String.lowercase_ascii with
      | "easy" -> Easy
      | "medium" -> Medium
      | "hard" -> Hard
      | _ -> failwith "Illegal Command"
    in
    let wealth =
      print_endline "\nEnter wealth assigned to PokerBot:";
      print_string "> ";
      try
        let raw = read_line () |> int_of_string in
        if raw < 0 then failwith "negative" else raw
      with Failure _ ->
        print_endline "Warning: wealth must be a nonnegative integer.";
        100
    in
    "PokerBot's initial wealth is $" ^ string_of_int wealth ^ "."
    |> print_endline;
    let player = create_player "PokerBot" wealth (i + 1) (true, level) in
    player :: ls
  with _ ->
    print_endline "Illegal Command";
    get_bot_level i ls

(** [parse_bot_cmd str] parses the string command and returns Command
and a corresponding string *)
let parse_bot_cmd str =
  match str with
  | [ "fold" ] -> (Fold, "Fold")
  | [ "call" ] | [ "raise"; "0" ] | [ "" ] -> (Call, "Call")
  | [ "raise"; n ] ->
      let new_n = int_of_string n in
      (Raise new_n, "Raise " ^ n)
  | _ -> failwith "Illegal Command"

(** [load_file] prompts user to enter game file and converts it to type
    game. x is dummy variable. Condition: requested json file exists in
    game_files, file in right format of a game file. *)
let load_file () : game =
  print_endline "Enter the game file: ";
  print_string "> ";
  let filename = read_line () in
  match
    Yojson.Basic.from_file ("game_files/" ^ filename ^ ".json")
    |> read_game
  with
  | exception Sys_error _ ->
      print_endline "file not found\n";
      raise (Exit 2)
  | exception BadFormat ->
      print_endline "bad json format\n";
      raise (Exit 2)
  | game -> game

let rec player_result_helper = function
  | [] -> print_string ""
  | h :: t ->
      player_result_helper t;
      name h ^ ": $" ^ string_of_int (wealth h) |> print_endline

(** [player_result] prints the naeme and wealth of all players *)
let rec player_result ls = List.rev ls |> player_result_helper

(** [parse] asks user to enter a parseable command. input of "exit"
    exits the game. Raise : Exit. *)
let parse game : command =
  print_string "> ";
  match
    String.(
      read_line () |> trim |> lowercase_ascii |> split_on_char ' ')
  with
  | [ "fold" ] -> Fold
  | [ "call" ] | [ "raise"; "0" ] | [ "" ] -> Call
  | [ "raise"; n ] ->
      let n = int_of_string n in
      if n > 0 then Raise n else failwith "Illegal Command"
  | [ "exit" ] -> Exit 0 |> raise
  | [ "save"; name ] ->
      if save_game game name then
        print_endline "\nGame saved to game_files folder."
      else print_endline "\nFailed to save game.";
      Exit 1 |> raise
  | _ -> failwith "Illegal Command"

(** [reshuffling_parse] asks user to enter command during reshuffling
    period. Raise: Exit. *)
let rec reshuffle_parse game : game =
  print_string "\n> ";
  try
    match
      String.(
        read_line () |> trim |> lowercase_ascii |> split_on_char ' ')
    with
    | [ "add"; "fund"; name; amount ] ->
        let game = add_fund game name (int_of_string amount) in
        print_endline "Action succeeded";
        reshuffle_parse game
    | [ "remove"; "player"; name ] -> 
        let game = remove_player game name in
        print_endline "Action succeeded";
        reshuffle_parse game
    | [ "add"; "player"; name; wealth ] ->
        let game = add_player game name (int_of_string wealth) in
        print_endline "Action succeeded";
        reshuffle_parse game
    | [ "start" ] -> play_again game
    | [ "status" ] ->
        player_result game.active_players;
        reshuffle_parse game
    | [ "exit" ] -> Exit 0 |> raise 
    | [ "save"; n ] ->
      if save_game game n then
        print_endline "\nGame saved to game_files folder."
      else print_endline "\nFailed to save game.";
      Exit 1 |> raise
    | _ -> failwith "Illegal Command"
  with
  | PlayerNotFound ->
      print_endline "Action failed: player not found";
      reshuffle_parse game
  | NotEnoughPlayers ->
      print_endline "Action failed: not enough players ";
      reshuffle_parse game
  | DuplicateName ->
      print_endline "Action failed: duplicate name";
      reshuffle_parse game
  | Failure _ ->
      print_endline
        "Action failed: amount must be a nonnegative integer";
      reshuffle_parse game

(** Prompts for command until get a valid command*)
let rec get_command game : game * int =
  legal_move game;
  try
    let command = parse game in
    execute_command game command
  with
  | Failure _ ->
      print_endline "Illegal Command";
      get_command game
  | InsufficientFund ->
      print_endline "Insufficient Fund";
      get_command game
  | RaiseFailure ->
      print_endline "Insufficient Raise";
      get_command game

(** [end_game] shows the result of the game and asks whether to play
    again *)
let rec end_game game =
  print_endline "\n\nThis game is over.";
  let winners = List.map (fun x -> name x) (get_winners game) in
  let s = if List.length winners = 1 then "Winner: " else "Winners: " in
  s ^ String.concat ", " winners |> print_endline;
  "Winning hand has " ^ get_winning_hand game ^ "." |> print_endline;
  print_endline "\nPlayer Status";
  let players = get_all_players game in
  player_result players;
  print_endline "\nWould you like to start another game? (Y/N)";
  print_string "> ";
  match String.(read_line () |> trim |> lowercase_ascii) with
  | "y" | "" -> reshuffling_period game |> reshuffle
  | _ ->
      print_endline "\nbye\n";
      exit 0

and reshuffle game =
  print_endline "\n\n\n\n\n\n\n\nReshuffling Period";
  print_endline
    "Commands Menu: \n\
    \ Add Fund [Name] [Amount] \n\
    \ Add Player [Name] [Wealth] \n\
    \ Remove Player [Name] \n\
    \ Status \n\
    \ Save [filename] \n\
    \ Exit \n\
    \ Start";
  reshuffle_parse game |> begin_play

(** [begin_play] displays commands menu and play. *)
and begin_play game =
  print_endline "\n\n\n\n\n\n\n\nGame Started";
  "Small Blind : " ^ name game.small_blind |> print_endline;
  print_endline "\nPlayer Status";
  get_all_players game |> player_result;
  print_endline
    "\n\
     Game Commands Menu: \n\
    \ Fold \n\
    \ Call \n\
    \ Raise [amount] \n\
    \ Save [filename] \n\
    \ Exit";
  play game

(** [play] loops through plyaers, displaying relevant information and
    asks for command*)
and play game =
  if List.length game.winners > 0 then end_game game
  else if game.new_round = true then begin
    print_endline "\n\n\n\n\n\n\n\nNew cards have been dealt. ";
    "Table: " ^ pretty_print game.cards_on_table |> print_endline;
    play { game with new_round = false }
  end
  else
    let p = get_curr_player game in
    "\n\n\n\n\n\n\n\nThe next player is " ^ name p ^ "."
    |> print_endline;
    let bot, mode = is_bot p in
    if bot then (
      let cmd_str_lst =
        next_move mode (cards p) (table game) game.current_deck
          (wealth p) game.minimum_raise
      in
      print_endline "PokerBot is thinking...";
      let cmd, cmd_str = parse_bot_cmd cmd_str_lst in
      "PokerBot plays " ^ cmd_str |> print_endline;
      let game, amount = execute_command game cmd in
      "$" ^ string_of_int amount ^ " to the pot." |> print_endline;
      play game)
    else print_endline "Press Enter to confirm.";
    print_string (read_line ());
    "\nTable: " ^ pretty_print game.cards_on_table |> print_endline;
    "\nHello, " ^ name p ^ "!" |> print_endline;
    "Your Hand: " ^ pretty_print (cards p) |> print_endline;
    "\nYour wealth is $" ^ string_of_int (wealth p) ^ "."
    |> print_endline;
    "The pot has $" ^ string_of_int game.pot ^ "." |> print_endline;
    "Highest bet on the table is $"
    ^ string_of_int game.current_bet
    ^ "."
    |> print_endline;
    "Your current bet is $" ^ string_of_int (amount_placed p) ^ "."
    |> print_endline;
    try
      let game, amount = get_command game in
      "$" ^ string_of_int amount ^ " to the pot." |> print_endline;
      play game
    with Exit n ->
      "\nexit code " ^ string_of_int n ^ "\n" |> print_endline;
      exit 0

(** [create_players n i ls namels] adds [n] players to [ls]. Prompts
    each player to enter in their names and initial wealth. Default
    player name: player[i]. Default wealth: 100. Default cards: a full
    deck. Checks that the new name cannot be in namels *)
let rec create_players n i (ls : player list) (namels : string list) =
  if i > n then (
    print_endline "\nWant to play with PokerBot? (Y/N)";
    print_string "> ";
    match read_line () |> String.trim |> String.lowercase_ascii with
    | "y" -> get_bot_level i ls
    | "n" -> ls
    | _ ->
        print_endline "Warning: No PokerBot in this game by defualt. \n";
        ls)
  else
    let _ =
      "\n*** Player" ^ string_of_int i ^ " ***" |> print_endline;
      print_endline "\nEnter your name: ";
      print_string "> "
    in
    let name =
      match read_line () with
      | "" ->
          print_endline "Warning: name cannot be empty.";
          "player" ^ string_of_int i
      | name ->
          if List.mem name namels then (
            print_endline "Warning: duplicate name.";
            "player" ^ string_of_int i)
          else name
    in
    "\nHello " ^ name ^ "!" |> print_endline;
    print_endline "Enter your wealth: ";
    print_string "> ";
    let wealth =
      try
        let raw = read_line () |> int_of_string in
        if raw < 0 then failwith "negative" else raw
      with Failure _ ->
        print_endline "Warning: wealth must be a nonnegative integer.";
        100
    in
    "Your initial wealth is $" ^ string_of_int wealth ^ "."
    |> print_endline;
    let player = create_player name wealth i (false, None) in
    create_players n (i + 1) (player :: ls) (name :: namels)

(** [setup] sets up the initial state of the a new game. *)
let setup () =
  print_endline "Please enter the number of players.\n";
  print_string "> ";
  let n =
    try
      let raw = read_line () |> int_of_string in
      if raw <= 0 then failwith "nonpositive" else raw
    with Failure _ ->
      print_endline
        "Warning: number of players is now set to 2 by default. \n";
      2
  in
  let players = create_players n 1 [] [] in
  print_endline "\nPlease enter the amount of small blind.\n";
  print_string "> ";
  let sb =
    try
      let raw = read_line () |> int_of_string in
      if raw < 0 then failwith "negative" else raw
    with Failure _ ->
      print_endline
        "Warning: small blind is now set to $5 by default. \n";
      5
  in
  "The small blind is $" ^ string_of_int sb ^ "." |> print_endline;
  let game = create_game (List.rev players) sb in
  print_endline "\n\nsetup completed";
  "small blind : " ^ name game.small_blind |> print_endline;
  print_endline "the blinds are placed by dealer\n";
  game

(** Exectue game enegine *)
let () =
  print_endline "\n\nWelcome to poker.\n";
  print_endline
    "Setup Commands: \n\
    \ New: start a new game \n\
    \ Load : load an existing JSON game file";
  print_string "> ";
  match read_line () |> String.trim |> String.lowercase_ascii with
  | "new" -> setup () |> begin_play
  | "load" -> 
    begin
      try load_file () |> begin_play
      with Exit n ->
        "\nexit code " ^ string_of_int n ^ "\n" |> print_endline
    end
  | _ -> ()
