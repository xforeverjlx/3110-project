open OUnit2
open Texas_holdem
open Card
open Game_with_bot

let index_of_highest_hand_test
    (name : string)
    (input : card list list)
    (expected_output : int) =
  name >:: fun _ ->
  assert_equal expected_output (index_of_highest_hand input)

let index_of_highest_hand_fail_test
    (name : string)
    (input : card list list)
    (expected_output : exn) =
  name >:: fun _ ->
  assert_raises expected_output (fun _ -> index_of_highest_hand input)

let hand0 = [ H 1; D 12; D 10; C 8; D 4; S 3; H 2 ]
(* high card, high = 1 *)

let hand0_match = [ H 1; D 12; S 10; D 8; S 4; D 2; C 3 ]
(* high card, high = 1, same as hand0 *)

let hand0_kicker_0 = [ H 1; D 12; S 11; D 9; C 5; C 3; S 2 ]
(* high card, high = 1, higher than hand0 *)

let hand0_kicker_1 = [ H 1; D 13; S 11; D 9; C 5; C 3; S 2 ]
(* high card, high = 1, higher than hand0_kicker_0 *)

let hand1 = [ H 1; S 1; H 10; S 9; D 6; H 3; D 4 ] (* a pair *)

let hand1_match_1 = [ H 11; S 11; H 10; S 9; H 3; D 6; D 4 ]
(* a pair, lower than hand1 *)

let hand1_match_2 = [ D 1; C 1; H 10; S 9; H 3; D 6; D 4 ]
(* a pair, same as hand1 *)

let hand1_kicker = [ D 1; C 1; H 10; S 9; D 8; H 3; D 4 ]
(* a pair, higher than hand1 *)

let hand2 = [ C 5; D 10; H 2; S 6; C 1; C 2; S 10 ] (* two pair *)

let hand2_match_0 = [ C 5; D 11; H 2; S 6; C 1; C 2; S 11 ]
(* two pair, higher than hand2 *)

let hand2_match_1 = [ C 5; D 10; H 3; S 6; C 1; C 3; S 10 ]
(* two pair, higher than hand2 *)

let hand2_kicker = [ C 5; D 10; H 2; S 6; C 9; C 2; S 10 ]
(* two pair, lower than hand2 *)

let hand3 = [ C 5; D 2; H 2; S 6; C 3; C 2; S 10 ] (* three of a kind *)

let hand3_match = [ S 10; D 10; H 10; D 12; C 5; D 8; S 7 ]
(* three of a kind, higher than hand3 *)

let hand3_kicker = [ C 1; D 2; H 2; S 6; C 3; C 2; S 10 ]
(* three of a kind, higher than hand3 *)

let hand4 = [ H 10; S 9; C 8; S 7; H 2; H 11; S 12 ] (* a straight *)

let hand4_match_0 = [ H 10; S 9; C 8; S 7; H 11; S 12; H 13 ]
(* a straight, higher than hand4 *)

let hand4_match_1 = [ C 2; S 3; C 4; D 4; H 5; C 6; D 1 ]
(* a straight *)

let hand4_match_2 = [ H 2; S 3; D 4; C 4; H 5; D 10; D 1 ]
(* a straight, lower than hand4_match_1 *)

let hand5 = [ C 1; C 11; C 5; C 3; C 2; H 8; S 6 ] (* flush *)

let hand5_match_0 = [ C 13; C 9; C 8; C 7; C 5; C 2; S 6 ]
(* flush, lower than hand5 *)

let hand5_match_1 = [ C 13; C 9; C 8; C 7; C 5; C 3; S 6 ]
(* flush, same as hand5_match_0 *)

let hand5_match_2 = [ D 1; D 11; D 5; D 3; D 2; C 8; H 6 ]
(* flush, same as hand5 *)

let hand5_kicker = [ C 13; C 10; C 4; C 3; C 2; H 8; S 6 ]
(* flush, higher than hand5_match_0 and hand5_match_1 *)

let hand6 = [ C 1; D 1; H 1; S 11; C 11; H 9; S 8 ] (* full house *)

let hand6_match_0 = [ C 2; D 2; H 2; S 11; C 11; H 9; S 8 ]
(* full house, lower than hand6 *)

let hand6_match_1 = [ C 1; D 1; S 9; H 3; C 9; D 9; H 11 ]
(* full house, higher than hand6_match_0, lower than hand6 *)

let hand7 = [ C 5; D 5; H 5; S 5; C 2; C 1; S 4 ] (* four of a kind *)

let hand7_match_0 = [ C 6; D 6; H 6; S 6; C 2; C 1; S 0 ]
(* four of a kind, higher than hand7 *)

let hand7_kicker = [ C 5; D 5; H 5; S 5; C 2; C 3; S 4 ]
(* four of a kind, lower than hand7 *)

let hand8 = [ C 9; C 10; C 11; C 12; C 13; S 8; D 3 ]
(* straight flush *)

let hand8_match = [ D 8; D 9; D 10; D 11; D 12; D 2; D 3 ]
(* straight flush, lower than hand8 *)

let hand9 = [ D 10; D 13; D 1; D 12; D 11; C 11; H 1 ] (* royal flush *)

let hand9_match = [ C 10; C 13; C 1; C 12; C 11; D 11; D 1 ]
(* royal flush *)

(*(* tests for game*)

  let test1 = [ H 3; C 6; H 8; H 5; S 2; D 10; S 7 ] let test2 = [ H 3;
  H 2; H 5; H 8; C 7; S 2; C 6 ] let test3 = [ H 3; H 4; H 5; H 8; D 12;
  S 2; C 6 ] let buggy_hand_0 = [ S 2; H 7; C 12; H 12; C 9; D 1; H 5 ]
  let buggy_hand_1 = [ S 2; H 7; C 12; H 12; C 9; D 2; S 5 ]*)

let starting_hand_estimated_strength_test
    (name : string)
    (input : card list)
    (expected_output : int) =
  name >:: fun _ ->
  assert_equal expected_output (starting_hand_estimated_strength input)

let card_tests =
  [
    starting_hand_estimated_strength_test "strength 5" [ C 13; D 1 ] 5;
    starting_hand_estimated_strength_test "strength 4" [ D 1; H 12 ] 4;
    starting_hand_estimated_strength_test "strength 3" [ C 1; C 2 ] 3;
    starting_hand_estimated_strength_test "strength 2" [ D 6; D 5 ] 2;
    starting_hand_estimated_strength_test "strength 1" [ C 9; D 7 ] 1;
    starting_hand_estimated_strength_test "strength 0" [ S 10; H 2 ] 0;
    index_of_highest_hand_test
      "index_of_highest_hand_test hand4_match_1 hand4_match_2"
      [ hand4_match_1; hand4_match_2 ]
      0;
    index_of_highest_hand_test
      "index_of_highest_hand_test hand3 hand2 hand5"
      [ hand3; hand5; hand2 ] 1;
    index_of_highest_hand_test
      "index_of_highest_hand_test hand5 hand7 hand8"
      [ hand5; hand7; hand8 ] 2;
    index_of_highest_hand_test
      "index_of_highest_hand_test hand8 hand6 hand9"
      [ hand8; hand6; hand9 ] 2;
    index_of_highest_hand_test "a small pair and a high"
      [ hand0; hand1 ] 1;
    index_of_highest_hand_test "index_of_highest_hand_test hand0-9"
      [
        hand0;
        hand1;
        hand2;
        hand3;
        hand4;
        hand5;
        hand6;
        hand8;
        hand7;
        hand9;
      ]
      9;
    (* Below are tests for kickers. *)
    index_of_highest_hand_test
      "index_of_highest_hand_test hand0 hand0_kicker_0 hand0_kicker_1"
      [ hand0; hand0_kicker_0; hand0_kicker_1 ]
      2;
    index_of_highest_hand_test
      "index_of_highest_hand_test hand1 hand1_match_1 hand1_kicker"
      [ hand1; hand1_match_1; hand1_kicker ]
      2;
    index_of_highest_hand_test
      "index_of_highest_hand_test hand2 hand2_kicker"
      [ hand2; hand2_kicker ] 0;
    index_of_highest_hand_test
      "index_of_highest_hand_test hand3 hand3_kicker"
      [ hand3; hand3_kicker ] 1;
    index_of_highest_hand_test
      "index_of_highest_hand_test hand5_match_0 hand5_kicker"
      [ hand5_match_0; hand5_kicker ]
      1;
    index_of_highest_hand_test
      "index_of_highest_hand_test hand7 hand7_kicker"
      [ hand7; hand7_kicker ] 0;
    (* Below are tests for tie or tie breakers. *)
    (* test of high card *)
    index_of_highest_hand_fail_test "0 tie_test hand0 hand0_match"
      [ hand0; hand0_match ]
      (Tie [ 1; 0 ]);
    (* test of pair *)
    index_of_highest_hand_test "1 tie_test hand1 hand1_match_1"
      [ hand1; hand1_match_1 ]
      0;
    index_of_highest_hand_fail_test
      "1 tie_test hand1 hand1_match_1 hand1_match_2"
      [ hand1; hand1_match_1; hand1_match_2 ]
      (Tie [ 2; 0 ]);
    (* test of two pair *)
    index_of_highest_hand_test "2 tie_test hand2 hand2_match_1"
      [ hand2; hand2_match_1 ]
      1;
    index_of_highest_hand_test
      "2 tie_test hand2 hand2_match_0 hand2_match_1"
      [ hand2; hand2_match_0; hand2_match_1 ]
      1;
    (* test of three of a kind *)
    index_of_highest_hand_test "3 tie_test hand3 hand3_match"
      [ hand3; hand3_match ] 1;
    index_of_highest_hand_fail_test
      "3 tie_test hand3 hand3_match hand3_match"
      [ hand3; hand3_match; hand3_match ]
      (Tie [ 1; 1 ]);
    (* test of straight *)
    index_of_highest_hand_test "4 tie_test hand4 hand4_match_0"
      [ hand4_match_0; hand4; hand3 ]
      0;
    index_of_highest_hand_fail_test "4 tie_test hand4 hand4"
      [ hand4; hand4 ]
      (Tie [ 0; 0 ]);
    (* test of flush *)
    index_of_highest_hand_test "5 tie_test hand5 hand5_match_0"
      [ hand5; hand5_match_0 ]
      0;
    index_of_highest_hand_fail_test "5 tie_test hand5 hand5_match_2"
      [ hand5; hand5_match_2 ]
      (Tie [ 1; 0 ]);
    index_of_highest_hand_fail_test
      "5 tie_test hand5_match_0 hand5_match_1"
      [ hand5_match_0; hand5_match_1 ]
      (Tie [ 1; 0 ]);
    (* test of full house *)
    index_of_highest_hand_test
      "6 tie_test hand6 hand6_match_0 hand6_match_1"
      [ hand6; hand6_match_0; hand6_match_1 ]
      0;
    (* test of four of a kind *)
    index_of_highest_hand_test "7 tie_test hand7 hand7_match_0"
      [ hand7; hand7_match_0 ]
      1;
    index_of_highest_hand_fail_test "7 tie_test hand3 hand7 hand7"
      [ hand3; hand7; hand7 ]
      (Tie [ 1; 1 ]);
    (* test of straight flush *)
    index_of_highest_hand_test "8 tie_test hand8 hand8_match"
      [ hand8; hand8_match ] 0;
    index_of_highest_hand_fail_test "8 tie_test hand8 hand8"
      [ hand8; hand8 ]
      (Tie [ 0; 0 ]);
    (* test of royal flush *)
    index_of_highest_hand_fail_test "9 tie_test hand9 hand9_match"
      [ hand9; hand9_match ]
      (Tie [ 1; 0 ])
    (*index_of_highest_hand_test "testing buggy hands" [ buggy_hand_0;
      buggy_hand_1 ] 1; index_of_highest_hand_test "testing game
      outputs" [ test1; test2; test3 ] 2;*);
  ]

let single_compare_test
    (name : string)
    (input1 : card)
    (input2 : card)
    (expected_output : int) =
  name >:: fun _ ->
  assert_equal expected_output (single_compare input1 input2)

let f_test
    (name : string)
    (f : card list -> bool)
    (input : card list)
    (expected_output : bool) =
  name >:: fun _ -> assert_equal expected_output (f input)

let card_impl_tests =
  [
    single_compare_test "single_compare_test greater" (C 1) (H 2) 1;
    single_compare_test "single_compare_test lesser" (D 5) (S 13) ~-1;
    single_compare_test "single_compare_test equal 1" (H 13) (H 13) 0;
    single_compare_test "single_compare_test equal 2" (H 13) (C 13) 0;
    f_test "has_pair_test hand1" has_pair hand3 true;
    f_test "has_pair_test hand2" has_pair hand2 true;
    f_test "has_pair_test hand5" has_pair hand5 false;
    f_test "has_pair_test hand7" has_pair hand7 true;
    f_test "has_two_pair_test hand1" has_two_pair hand3 false;
    f_test "has_two_pair_test hand2" has_two_pair hand2 true;
    f_test "has_two_pair_test hand5" has_two_pair hand5 false;
    f_test "has_two_pair_test hand7" has_two_pair hand7 false;
    f_test "has_three_of_a_kind_test hand1" has_three_of_a_kind hand3
      true;
    f_test "has_three_of_a_kind_test hand2" has_three_of_a_kind hand2
      false;
    f_test "has_three_of_a_kind_test hand5" has_three_of_a_kind hand5
      false;
    f_test "has_three_of_a_kind_test hand7" has_three_of_a_kind hand7
      true;
    f_test "has_four_of_a_kind_test hand1" has_four_of_a_kind hand3
      false;
    f_test "has_four_of_a_kind_test hand2" has_four_of_a_kind hand2
      false;
    f_test "has_four_of_a_kind_test hand5" has_four_of_a_kind hand5
      false;
    f_test "has_four_of_a_kind_test hand7" has_four_of_a_kind hand7 true;
    f_test "has_four_of_a_kind_test hand8" has_four_of_a_kind hand8
      false;
    f_test "has_four_of_a_kind_test hand6" has_four_of_a_kind hand6
      false;
    f_test "has_four_of_a_kind_test hand9" has_four_of_a_kind hand9
      false;
    f_test "has_four_of_a_kind_test hand6_match_1" has_four_of_a_kind
      hand6_match_1 false;
    f_test "has_straight_test hand8" has_straight hand8 true;
    f_test "has_straight_test hand7" has_straight hand7 false;
    f_test "has_straight_test hand5" has_straight hand5 false;
    f_test "has_straight_test hand2" has_straight hand2 false;
    f_test "has_straight_test hand1" has_straight hand3 false;
    f_test "has_straight" has_straight hand4_match_2 true;
    f_test "has_flush_test hand5" has_flush hand5 true;
    f_test "has_flush_test hand2" has_flush hand2 false;
    f_test "has_flush_test hand1" has_flush hand3 false;
    f_test "has_full_house_test hand6" has_full_house hand6 true;
    f_test "has_full_house_test hand5" has_full_house hand5 false;
    f_test "has_straight_flush_test hand8" has_straight_flush hand8 true;
    f_test "has_straight_flush_test hand7" has_straight_flush hand7
      false;
    f_test "has_straight_flush_test hand6" has_straight_flush hand6
      false;
    f_test "has_royal_flush_test hand9" has_royal_flush hand9 true;
    f_test "has_royal_flush_test hand5" has_royal_flush hand5 false;
  ]

let get_small_blind_test
    (name : string)
    (input : game)
    (expected_output : string) =
  name >:: fun _ ->
  assert_equal expected_output (Player_with_bot.name input.small_blind)

let get_curr_player_test
    (name : string)
    (input : game)
    (expected_output : string) =
  name >:: fun _ ->
  assert_equal expected_output
    (Player_with_bot.name (Game_with_bot.get_curr_player input))

let player_a = Player_with_bot.create_player_full "b" 20 [ D 1; H 5 ] 0 0 (false, None)
let player_b = Player_with_bot.create_player_full "a" 20 [ D 2; S 5 ] 0 1 (false, None)
let player_c = Player_with_bot.create_player_full "c" 30 [] 0 2 (false, None)
let player_d = Player_with_bot.create_player_full "d" 30 [] 0 3 (false, None)

let rec list_to_queue players queue =
  match players with
  | [] -> queue
  | h :: t ->
      list_to_queue t
        (Queue.add h queue;
         queue)

let queue = Queue.create ()
let players_queue = list_to_queue [ player_a; player_b ] queue
let queue1 = Queue.create ()

let players_queue1 =
  list_to_queue [ player_a; player_b; player_c; player_d ] queue1

let queue2 = Queue.create ()

let players_queue2 =
  list_to_queue [ player_b; player_c; player_d; player_a ] queue2

let queue3 = Queue.create ()

let players_queue3 =
  list_to_queue [ player_c; player_d; player_a; player_b ] queue3

let players_queue_with_b_only = list_to_queue [ player_b ] queue
let players_queue_with_a_only = list_to_queue [ player_a ] queue
let player_list2 = [ player_a; player_b ]
let player_list4 = [ player_a; player_b; player_c; player_d ]
let g_by_init2 = create_game player_list2 5
let folded_g2, _ = execute_command g_by_init2 Fold
let g_by_init4 = create_game player_list4 5
let folded_g4, _ = execute_command g_by_init4 Fold
let folded_g4, _ = execute_command folded_g4 Fold

let save_game_test
    (name : string)
    (input : game)
    (name : string)
    (expected_output : bool) =
  name >:: fun _ ->
  assert_equal expected_output (Game_with_bot.save_game input name)

let read_game_test
    (name : string)
    (input : string)
    (expected_output : int) =
  name >:: fun _ ->
  assert_equal expected_output
    (Game.read_game (Yojson.Basic.from_file input)).pot

let game_save_read_tests =
  [
    save_game_test "" g_by_init2 "2player" true;
    save_game_test "" folded_g2 "2playerfold" true;
    save_game_test "" folded_g4 "4playerfold" true;
    read_game_test "" "game_files/2player.json" 15;
    read_game_test "" "game_files/2playerfold.json" 15;
    read_game_test "" "game_files/4playerfold.json" 15;
  ]

let suite =
  "test suite for texas_holdem"
  >::: List.flatten
         [ card_tests; card_impl_tests; game_save_read_tests ]

let _ = run_test_tt_main suite
