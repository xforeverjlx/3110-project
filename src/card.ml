type card =
  | S of int
  | H of int
  | C of int
  | D of int

type t = card list

let string_of_rank = function
  | 11 -> "J"
  | 12 -> "Q"
  | 13 -> "K"
  | 1 -> "A"
  | x -> string_of_int x

let rec to_string (hand : t) : string =
  match hand with
  | [] -> ""
  | h :: t -> (
      match h with
      | S x -> string_of_rank x ^ "♠" ^ " " ^ to_string t
      | C x -> string_of_rank x ^ "♣" ^ " " ^ to_string t
      | H x -> string_of_rank x ^ "♥" ^ " " ^ to_string t
      | D x -> string_of_rank x ^ "♦" ^ " " ^ to_string t)

let rec first_last_line (n : int) (acc : string) : string =
  if n = 0 then acc else first_last_line (n - 1) (acc ^ "+ --- +   ")

let rec second_line (lst : string list) (acc : string) : string =
  if List.length lst = 0 then acc
  else
    let value = List.hd lst in
    if String.length value = 1 then
      second_line (List.tl lst) (acc ^ "|" ^ value ^ "    |   ")
    else second_line (List.tl lst) (acc ^ "|" ^ value ^ "   |   ")

let rec third_line (lst : string list) (acc : string) : string =
  if List.length lst = 0 then acc
  else third_line (List.tl lst) (acc ^ "|  " ^ List.hd lst ^ "  |   ")

let rec fourth_line (lst : string list) (acc : string) : string =
  if List.length lst = 0 then acc
  else
    let value = List.hd lst in
    if String.length value = 1 then
      fourth_line (List.tl lst) (acc ^ "|    " ^ value ^ "|   ")
    else fourth_line (List.tl lst) (acc ^ "|   " ^ value ^ "|   ")

let extract_value (c : card) =
  match c with
  | S x | H x | C x | D x -> x

let rec pretty_print (hand : t) : string =
  let n = List.length hand in
  let value_list =
    List.map string_of_rank (List.map extract_value hand)
  in
  let suit_list =
    List.map
      (fun x ->
        match x with
        | S x -> "♠"
        | C x -> "♣"
        | H x -> "♥"
        | D x -> "♦")
      hand
  in
  "\n" ^ first_last_line n "" ^ "\n"
  ^ second_line value_list ""
  ^ "\n" ^ third_line suit_list "" ^ "\n"
  ^ fourth_line value_list ""
  ^ "\n" ^ first_last_line n ""

let rec create_new_deck lst =
  match lst with
  | [] -> create_new_deck [ S 1 ]
  | h :: t -> (
      match h with
      | S x ->
          if x = 13 then create_new_deck (H 1 :: lst)
          else create_new_deck (S (x + 1) :: lst)
      | H x ->
          if x = 13 then create_new_deck (C 1 :: lst)
          else create_new_deck (H (x + 1) :: lst)
      | C x ->
          if x = 13 then create_new_deck (D 1 :: lst)
          else create_new_deck (C (x + 1) :: lst)
      | D x -> if x = 13 then lst else create_new_deck (D (x + 1) :: lst)
      )

let new_deck = create_new_deck []

let random_card (t : t) =
  let _ = Random.self_init () in
  let c = List.nth t (Random.int (List.length t)) in
  let new_deck = List.filter (fun x -> x <> c) t in
  (c, new_deck)

let rec n_random_card (t : t) (n : int) =
  match n with
  | 0 -> ([], t)
  | _ ->
      let card, new_deck = random_card t in
      let remaining_cards, new_deck = n_random_card new_deck (n - 1) in
      (card :: remaining_cards, new_deck)

let single_compare (card1 : card) (card2 : card) =
  let x1 = extract_value card1 in
  let x2 = extract_value card2 in
  match (x1, x2) with
  | 1, 1 | 14, 14 | 1, 14 | 14, 1 -> 0
  | 1, _ | 14, _ -> 1
  | _, 1 | _, 14 -> -1
  | _ -> if x1 > x2 then 1 else if x1 < x2 then -1 else 0

let equal (card1 : card) (card2 : card) =
  match (card1, card2) with
  | C x1, C x2 | D x1, D x2 | H x1, H x2 | S x1, S x2 -> x1 = x2
  | _ -> false

let sort_and_rev (hand : t) =
  List.rev (List.stable_sort single_compare hand)

(** Returns an int list of non-unique elements found in [hand]. The
    actual number of cards of value v in [hand] is the number of
    occurrences of v in [determine_pair hand] + 1. Examples:
    [hand \[ C 5; D 2; H 2; C 2; S 10; H 11 \]] is [2;2]*)
let rec determine_pair (hand : t) =
  let hand = sort_and_rev hand in
  let intlist = List.map extract_value hand in
  match intlist with
  | x1 :: x2 :: _ ->
      if x1 = x2 then
        extract_value (List.hd hand) :: determine_pair (List.tl hand)
      else determine_pair (List.tl hand)
  | _ -> []

let has_pair (hand : t) = determine_pair hand <> []

let has_two_pair (hand : t) =
  List.length (List.sort_uniq compare (determine_pair hand)) > 1

let has_three_of_a_kind (hand : t) =
  let l = determine_pair hand in
  let rec has_three_of_a_kind_helper l =
    match l with
    | [] -> false
    | h :: t -> List.mem h t || has_three_of_a_kind_helper t
  in
  has_three_of_a_kind_helper l

let rec has_straight_helper l =
  if List.length l < 5 then (false, -1)
  else
    let a = List.hd l in
    let b = a + 1 in
    let c = b + 1 in
    let d = c + 1 in
    let e = d + 1 in
    let boo =
      List.nth l 1 = b
      && List.nth l 2 = c
      && List.nth l 3 = d
      && List.nth l 4 = e
    in
    if boo then (boo, e) else has_straight_helper (List.tl l)

let has_straight (hand : t) =
  let hand = List.sort_uniq single_compare hand in
  let intlist = List.map extract_value hand in
  let intlist_to_14 =
    List.map (fun x -> if x = 1 then 14 else x) intlist
  in
  let res1 = fst (has_straight_helper intlist_to_14) in
  if res1 then res1
  else
    let intlist = List.stable_sort compare intlist in
    let res2 = fst (has_straight_helper intlist) in
    res2

let has_flush_helper (hand : t) =
  let s =
    List.fold_left
      (fun count x ->
        match x with
        | S _ -> count + 1
        | _ -> count)
      0 hand
  in
  if s > 4 then (true, 'S')
  else
    let h =
      List.fold_left
        (fun count x ->
          match x with
          | H _ -> count + 1
          | _ -> count)
        0 hand
    in
    if h > 4 then (true, 'H')
    else
      let c =
        List.fold_left
          (fun count x ->
            match x with
            | C _ -> count + 1
            | _ -> count)
          0 hand
      in
      if c > 4 then (true, 'C')
      else
        let d =
          List.fold_left
            (fun count x ->
              match x with
              | D _ -> count + 1
              | _ -> count)
            0 hand
        in
        if d > 4 then (true, 'D') else (false, ' ')

let has_flush (hand : t) = fst (has_flush_helper hand)

let has_full_house (hand : t) =
  let l = determine_pair hand in
  let rec has_full_house_helper l b =
    match l with
    | h :: t ->
        if List.mem h t then
          b
          ||
          let rest = List.filter (fun x -> x != h) t in
          List.length rest > 0
        else has_full_house_helper t true
    | _ -> false
  in
  has_full_house_helper l false

let has_four_of_a_kind (hand : t) =
  let l = determine_pair hand in
  let l = List.stable_sort compare l in
  let rec has_four_of_a_kind_helper l =
    if List.length l < 3 then false
    else
      match l with
      | [] -> false
      | h :: t ->
          (List.mem h t && List.mem h (List.tl t))
          || has_four_of_a_kind_helper t
  in
  has_four_of_a_kind_helper l

let has_straight_flush (hand : t) =
  let suit = snd (has_flush_helper hand) in
  match suit with
  | 'S' ->
      hand
      |> List.filter (fun x ->
             match x with
             | S x -> true
             | _ -> false)
      |> has_straight
  | 'H' ->
      hand
      |> List.filter (fun x ->
             match x with
             | H x -> true
             | _ -> false)
      |> has_straight
  | 'C' ->
      hand
      |> List.filter (fun x ->
             match x with
             | C x -> true
             | _ -> false)
      |> has_straight
  | 'D' ->
      hand
      |> List.filter (fun x ->
             match x with
             | D x -> true
             | _ -> false)
      |> has_straight
  | _ -> false

let sort_and_group hand =
  let s =
    List.filter
      (fun x ->
        match x with
        | S x -> true
        | _ -> false)
      hand
  in
  let s = sort_and_rev s in
  let h =
    List.filter
      (fun x ->
        match x with
        | H x -> true
        | _ -> false)
      hand
  in
  let h = sort_and_rev h in
  let c =
    List.filter
      (fun x ->
        match x with
        | C x -> true
        | _ -> false)
      hand
  in
  let c = sort_and_rev c in
  let d =
    List.filter
      (fun x ->
        match x with
        | D x -> true
        | _ -> false)
      hand
  in
  let d = sort_and_rev d in
  s @ h @ c @ d

let has_royal_flush (hand : t) =
  let hand = sort_and_group hand in
  let rec has_royal_flush_helper hand =
    match hand with
    | S 1 :: S 13 :: S 12 :: S 11 :: S 10 :: _ -> true
    | H 1 :: H 13 :: H 12 :: H 11 :: H 10 :: _ -> true
    | C 1 :: C 13 :: C 12 :: C 11 :: C 10 :: _ -> true
    | D 1 :: D 13 :: D 12 :: D 11 :: D 10 :: _ -> true
    | _ ->
        if List.length hand < 5 then false
        else has_royal_flush_helper (List.tl hand)
  in
  has_royal_flush_helper hand

let single_value_copmare x y = single_compare (C x) (C y)

let max_of_list (lst : int list) =
  List.fold_left
    (fun x y -> if single_value_copmare x y > 0 then x else y)
    2 lst

let rec tally (lst : int list) curr =
  (* tallies the number of occurrences of each element in lst *)
  match lst with
  | [] -> curr
  | h :: t -> (
      let find = List.find_opt (fun x -> fst x = h) curr in
      match find with
      | None ->
          let curr = (h, 1) :: curr in
          tally t curr
      | Some x ->
          let l = List.filter (fun x -> fst x != h) curr in
          let curr = (fst x, snd x + 1) :: l in
          tally t curr)

let filter_by_occurrences (lst : int list) (n : int) : int list =
  let lst = List.sort compare lst in
  let lst = tally lst [] in
  let lst = List.filter (fun x -> snd x = n) lst in
  fst (List.split lst)

exception Tied of t list

let winning_factor (hand : t) (rank : int) =
  match rank with
  | 1 | 2 ->
      max_of_list
        (filter_by_occurrences (List.map extract_value hand) 2)
  | 3 | 6 ->
      max_of_list
        (filter_by_occurrences (List.map extract_value hand) 3)
  | 4 ->
      let hand = List.sort_uniq single_compare hand in
      let intlist = List.map extract_value hand in
      let intlist =
        List.map (fun x -> if x = 1 then 14 else x) intlist
      in
      if not (fst (has_straight_helper intlist)) then 5
      else
        let curr_max = snd (has_straight_helper intlist) in
        let tail = List.tl hand in
        if has_straight tail then
          let intlist = List.map extract_value tail in
          let intlist =
            List.map (fun x -> if x = 1 then 14 else x) intlist
          in
          let curr_max =
            max curr_max (snd (has_straight_helper intlist))
          in
          let tail = List.tl tail in
          if has_straight tail then
            let intlist = List.map extract_value tail in
            let intlist =
              List.map (fun x -> if x = 1 then 14 else x) intlist
            in
            let curr_max =
              max curr_max (snd (has_straight_helper intlist))
            in
            curr_max
          else curr_max
        else curr_max
  | 5 -> (
      let c = snd (has_flush_helper hand) in
      let high_card (hand : t) =
        let hand = sort_and_rev hand in
        let hand = List.map extract_value hand in
        if List.hd hand = 1 then 14 else List.hd hand
      in
      match c with
      | 'C' ->
          let l =
            List.filter
              (fun x ->
                match x with
                | C x -> true
                | _ -> false)
              hand
          in
          high_card l
      | 'D' ->
          let l =
            List.filter
              (fun x ->
                match x with
                | D x -> true
                | _ -> false)
              hand
          in
          high_card l
      | 'S' ->
          let l =
            List.filter
              (fun x ->
                match x with
                | S x -> true
                | _ -> false)
              hand
          in
          high_card l
      | 'H' ->
          let l =
            List.filter
              (fun x ->
                match x with
                | H x -> true
                | _ -> false)
              hand
          in
          high_card l
      | _ -> 0)
  | 7 ->
      max_of_list
        (filter_by_occurrences (List.map extract_value hand) 4)
  | 8 ->
      let hand = List.stable_sort single_compare hand in
      let intlist = List.map extract_value hand in
      let intlist =
        List.map (fun x -> if x = 1 then 14 else x) intlist
      in
      let curr_max = snd (has_straight_helper intlist) in
      let tail = List.tl hand in
      if has_straight_flush tail then
        let intlist = List.map extract_value tail in
        let intlist =
          List.map (fun x -> if x = 1 then 14 else x) intlist
        in
        let curr_max =
          max curr_max (snd (has_straight_helper intlist))
        in
        let tail = List.tl tail in
        if has_straight_flush tail then
          let intlist = List.map extract_value tail in
          let intlist =
            List.map (fun x -> if x = 1 then 14 else x) intlist
          in
          let curr_max =
            max curr_max (snd (has_straight_helper intlist))
          in
          curr_max
        else curr_max
      else curr_max
  | _ -> 0

let rec check_unique lst ele seen =
  (* tests if ele is unique in lst *)
  match lst with
  | [] -> seen
  | h :: t ->
      if ele = h && not seen then check_unique t ele true
      else if ele = h && seen then false
      else check_unique t ele seen

let rec find_index_of_element e list index =
  match list with
  | [] -> -1
  | h :: t ->
      if h = e then index else find_index_of_element e t (index + 1)

let high_card_kicker (lst : t list) =
  let lst = List.map sort_and_rev lst in
  let list = List.map (fun x -> List.map extract_value x) lst in
  let list =
    List.map
      (fun x ->
        match x with
        | a :: b :: c :: d :: e :: _ -> [ a; b; c; d; e ]
        | _ -> failwith "")
      list
  in
  let temp =
    List.fold_left
      (fun curr_max x ->
        if List.compare single_value_copmare x curr_max > 0 then x
        else curr_max)
      [] list
  in
  if check_unique list temp false then
    let index = find_index_of_element temp list 0 in
    List.nth lst index
  else
    raise
      (Tied
         (List.filter
            (fun x ->
              let x = List.map extract_value x in
              let x =
                match x with
                | a :: b :: c :: d :: e :: _ -> [ a; b; c; d; e ]
                | _ -> failwith ""
              in
              x = temp)
            lst))

let pair_kicker (lst : t list) =
  let lst = List.map sort_and_rev lst in
  let list =
    List.map
      (fun x ->
        let winf = winning_factor x 1 in
        List.filter (fun x -> single_compare (C winf) x <> 0) x)
      lst
  in
  let list = List.map (fun x -> List.map extract_value x) list in
  let list =
    List.map
      (fun x ->
        match x with
        | a :: b :: c :: _ -> [ a; b; c ]
        | _ -> failwith "")
      list
  in
  let temp =
    List.fold_left
      (fun curr_max x ->
        if List.compare single_value_copmare x curr_max > 0 then x
        else curr_max)
      [] list
  in
  if check_unique list temp false then
    let index = find_index_of_element temp list 0 in
    List.nth lst index
  else
    raise
      (Tied
         (List.filter
            (fun x ->
              let x =
                let winf = winning_factor x 1 in
                List.filter (fun x -> single_compare (C winf) x <> 0) x
              in
              let x = sort_and_rev x in
              let x = List.map (fun x -> extract_value x) x in
              let x =
                match x with
                | a :: b :: c :: _ -> [ a; b; c ]
                | _ -> failwith ""
              in
              List.equal (fun x y -> compare x y = 0) temp x)
            lst))

let two_pair_kicker (lst : t list) : t =
  let lst = List.map (fun x -> sort_and_rev x) lst in
  let hand =
    List.map
      (fun x ->
        let winf = winning_factor x 2 in
        List.filter (fun x -> single_compare (C winf) x <> 0) x)
      lst
  in
  let winning_factors = List.map (fun x -> winning_factor x 1) hand in
  let max = max_of_list winning_factors in
  if check_unique winning_factors max false then
    let index = find_index_of_element max winning_factors 0 in
    List.nth lst index
  else
    let hand =
      List.map
        (fun x ->
          let winf = winning_factor x 1 in
          List.filter (fun x -> single_compare (C winf) x <> 0) x)
        hand
    in
    let rec high_card_extract lst =
      match lst with
      | [] -> []
      | h :: t -> List.hd (sort_and_rev h) :: high_card_extract t
    in
    let hand = high_card_extract hand in
    try
      let ele =
        List.fold_left
          (fun curr_max x ->
            if single_compare x curr_max > 0 then x
            else if single_compare x curr_max < 0 then curr_max
            else failwith (string_of_int (extract_value curr_max)))
          (List.hd hand) (List.tl hand)
      in
      List.find (fun x -> List.mem ele x) lst
    with Failure s ->
      let max = int_of_string s in
      let l =
        List.filter
          (fun x -> List.mem max (List.map extract_value x))
          lst
      in
      raise (Tied l)

let three_of_a_kind_kicker (lst : t list) =
  let lst = List.map (fun x -> sort_and_rev x) lst in
  let list =
    List.map
      (fun x ->
        let winf = winning_factor x 3 in
        List.filter (fun x -> single_compare (C winf) x <> 0) x)
      lst
  in
  let list = List.map (fun x -> List.map extract_value x) list in
  let list =
    List.map
      (fun x ->
        match x with
        | a :: b :: _ -> [ a; b ]
        | _ -> failwith "")
      list
  in
  let temp =
    List.fold_left
      (fun curr_max x ->
        if List.compare single_value_copmare x curr_max > 0 then x
        else curr_max)
      [] list
  in
  if check_unique list temp false then
    let index = find_index_of_element temp list 0 in
    List.nth lst index
  else
    raise
      (Tied
         (List.filter
            (fun x ->
              let x =
                let winf = winning_factor x 3 in
                List.filter (fun x -> single_compare (C winf) x <> 0) x
              in
              let x = sort_and_rev x in
              let x = List.map (fun x -> extract_value x) x in
              let x =
                match x with
                | a :: b :: _ -> [ a; b ]
                | _ -> failwith ""
              in
              List.equal (fun x y -> compare x y = 0) temp x)
            lst))

let flush_kicker (lst : t list) =
  let lst = List.map sort_and_rev lst in
  let list =
    List.map
      (fun x ->
        let c = snd (has_flush_helper x) in
        List.filter
          (fun x ->
            match x with
            | C _ -> c = 'C'
            | D _ -> c = 'D'
            | S _ -> c = 'S'
            | H _ -> c = 'H')
          x)
      lst
  in
  let list = List.map (fun x -> List.map extract_value x) list in
  let list =
    List.map
      (fun x ->
        match x with
        | a :: b :: c :: d :: e :: _ -> [ a; b; c; d; e ]
        | _ -> failwith "")
      list
  in
  let temp =
    List.fold_left
      (fun curr_max x ->
        if List.compare single_value_copmare x curr_max > 0 then x
        else curr_max)
      [] list
  in
  if check_unique list temp false then
    let index = find_index_of_element temp list 0 in
    List.nth lst index
  else
    raise
      (Tied
         (List.filter
            (fun x ->
              let c = snd (has_flush_helper x) in
              let x =
                List.filter
                  (fun x ->
                    match x with
                    | C _ -> c = 'C'
                    | D _ -> c = 'D'
                    | S _ -> c = 'S'
                    | H _ -> c = 'H')
                  x
              in
              let x = List.map extract_value x in
              let x =
                match x with
                | a :: b :: c :: d :: e :: _ -> [ a; b; c; d; e ]
                | _ -> failwith ""
              in
              x = temp)
            lst))

let four_of_a_kind_kicker (lst : t list) =
  let lst = List.map (fun x -> sort_and_rev x) lst in
  let list =
    List.map
      (fun x ->
        let winf = winning_factor x 7 in
        List.filter (fun x -> single_compare (C winf) x <> 0) x)
      lst
  in
  let list = List.map (fun x -> List.map extract_value x) list in
  let list = List.map (fun x -> [ List.hd x ]) list in
  let temp =
    List.fold_left
      (fun curr_max x ->
        if List.compare single_value_copmare x curr_max > 0 then x
        else curr_max)
      [] list
  in
  if check_unique list temp false then
    let index = find_index_of_element temp list 0 in
    List.nth lst index
  else
    raise
      (Tied
         (List.filter
            (fun x ->
              let x =
                let winf = winning_factor x 7 in
                List.filter (fun x -> single_compare (C winf) x <> 0) x
              in
              let x = sort_and_rev x in
              let x = List.map (fun x -> extract_value x) x in
              let x = [ List.hd x ] in
              List.equal (fun x y -> compare x y = 0) temp x)
            lst))

let rec break_tie (lst : t list) (rank : int) : t =
  (* elements in [lst] are tied at [rank], returns ele in [lst] that win
     out or raise Tied *)
  if rank = 9 then raise (Tied lst)
  else if rank = 0 then high_card_kicker lst
  else
    let winning_factors =
      List.map (fun x -> winning_factor x rank) lst
    in
    let max = max_of_list winning_factors in
    if check_unique winning_factors max false then
      let rec find_index_of_element e list index =
        match list with
        | [] -> -1
        | h :: t ->
            if h = e then index
            else find_index_of_element e t (index + 1)
      in
      let index = find_index_of_element max winning_factors 0 in
      List.nth lst index
    else
      let mapping_list = List.map (fun x -> x = max) winning_factors in
      let rec list_matching_extract map l =
        match (map, l) with
        | h1 :: t1, h2 :: t2 ->
            if h1 then h2 :: list_matching_extract t1 t2
            else list_matching_extract t1 t2
        | _ -> []
      in
      let extracted = list_matching_extract mapping_list lst in
      if rank = 1 then pair_kicker extracted
      else if rank = 2 then two_pair_kicker extracted
      else if rank = 3 then three_of_a_kind_kicker extracted
      else if rank = 5 then flush_kicker extracted
      else if rank = 7 then four_of_a_kind_kicker extracted
      else raise (Tied extracted)

let rank_of_hand (hand : t) =
  if has_royal_flush hand then 9
  else if has_straight_flush hand then 8
  else if has_four_of_a_kind hand then 7
  else if has_full_house hand then 6
  else if has_flush hand then 5
  else if has_straight hand then 4
  else if has_three_of_a_kind hand then 3
  else if has_two_pair hand then 2
  else if has_pair hand then 1
  else 0

let rec rank_hands (lst : t list) =
  match lst with
  | [] -> []
  | h :: t -> (rank_of_hand h, h) :: rank_hands t

let highest_hand_helper (lst : t list) =
  if List.length lst = 1 then List.hd lst
  else
    let lst = rank_hands lst in
    (*let _ = print_endline ("Ranks of hands: " ^ String.concat ", "
      (List.map string_of_int (fst (List.split lst)))) in*)
    let lst =
      List.rev
        (List.sort
           (fun x y ->
             if fst x > fst y then 1
             else if fst x = fst y then 0
             else -1)
           lst)
    in
    if fst (List.hd lst) != fst (List.nth lst 1) then snd (List.hd lst)
    else
      let lst = List.filter (fun x -> fst x = fst (List.hd lst)) lst in
      break_tie (snd (List.split lst)) (fst (List.hd lst))

exception Tie of int list

let rec find_index e list index =
  match list with
  | [] -> -1
  | h :: t ->
      if List.sort compare h = List.sort compare e then index
      else find_index e t (index + 1)

let index_of_highest_hand (lst : t list) =
  try
    let hand = highest_hand_helper lst in
    find_index hand lst 0
  with Tied list ->
    let l = List.map (fun x -> find_index x lst 0) list in
    raise (Tie l)

let same_suit (c1 : card) (c2 : card) : bool =
  match (c1, c2) with
  | C _, C _ | D _, D _ | S _, S _ | H _, H _ -> true
  | _, _ -> false

let starting_hand_estimated_strength (hand : t) =
  let values =
    List.rev
      (List.sort single_value_copmare (List.map extract_value hand))
  in
  let b = same_suit (List.hd hand) (List.nth hand 1) in
  match values with
  | [ 1; 1 ] | [ 1; 13 ] | [ 13; 13 ] | [ 12; 12 ] -> 5
  | [ 1; 12 ] | [ 13; 12 ] | [ 11; 11 ] | [ 10; 10 ] -> 4
  | [ 1; 11 ] -> if b then 4 else 3
  | [ 13; 11 ] | [ 12; 11 ] | [ 9; 9 ] | [ 8; 8 ] -> 3
  | [ 13; 10 ]
  | [ 1; 10 ]
  | [ 1; 9 ]
  | [ 1; 8 ]
  | [ 1; 7 ]
  | [ 1; 6 ]
  | [ 1; 5 ] ->
      if b then 3 else 2
  | [ 1; 4 ] | [ 1; 3 ] | [ 1; 2 ] -> if b then 3 else 1
  | [ 12; 10 ]
  | [ 11; 10 ]
  | [ 7; 7 ]
  | [ 6; 6 ]
  | [ 5; 5 ]
  | [ 4; 4 ]
  | [ 3; 3 ]
  | [ 2; 2 ] ->
      2
  | [ 13; 9 ]
  | [ 12; 9 ]
  | [ 11; 9 ]
  | [ 10; 9 ]
  | [ 10; 8 ]
  | [ 9; 8 ]
  | [ 8; 7 ]
  | [ 7; 6 ]
  | [ 6; 5 ] ->
      if b then 2 else 1
  | [ 11; 8 ] | [ 9; 7 ] | [ 8; 6 ] -> 1
  | [ 13; 8 ]
  | [ 12; 8 ]
  | [ 10; 7 ]
  | [ 9; 6 ]
  | [ 7; 5 ]
  | [ 6; 4 ]
  | [ 5; 4 ]
  | [ 4; 3 ] ->
      if b then 1 else 0
  | _ -> 0