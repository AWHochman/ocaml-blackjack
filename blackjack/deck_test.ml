(** 
   [TESTING PLAN] 
    -----------------------------------------------------------------------
    Given the randomness of drawing new cards through various stages of our 
    gameplay (initalizing a hand, hitting, etc.) we decided to test the 
    functionality of our gameplay in three main stages. First, we developed 
    and tested the ability to create each of the 52 cards in a standard
    deck. Then, we tested that we could add together the sums of these cards
    according to standard card summation rules. Second, we developed and 
    tested that we were able to create a 52-card deck as intended. Given 
    that we decided to implement the deck used as effectively infinite, we 
    didn't have to test removing a card from the deck because we instead 
    sample without replacement from this standard 52-card deck throughout 
    our gameplay. Finally, we had to test the intricacies of gameplay such as
    creating a state (which was our way of creating track of all the current 
    game's details), updating a state, handling betting, and printing out 
    game updates as the game is carried out. Through testing these main 
    components, we also tested the majority of our helper functions that were
    used to carry out the backend behavior of our game. These final state-
    related tests involved creating states that reprepresented both typical
    and edge-case games, and we made sure that the major components of gameplay
    were carried out as intended for each of these test states we created. 
    Thus, even though we don't directly test letting a program cycle through 
    our code, we feel confident that these tests encompass all of the aspects 
    of gameplay, and that all games will carry out in the manner we intend. 
*)

open OUnit2
open Deck
open Card
open Player
open State
open Hard_ai

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list (pp_elt : 'a -> string) (lst : 'a list) : string =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
        if n = 100 then acc ^ "..."  (* stop printing long list *)
        else loop (n + 1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

(** [card_attribute_test name card_attrib suit value expected_output printer]
    constructs an OUnit test named [name] that asserts the quality of 
    [expected_output] [create_card suit value |> card_attribute]. *)
let card_attribute_test
    (name : string) (card_attrib) (suit : suit) (value : value) 
    (expected_output) (printer : 'a -> string): test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal expected_output (create_card suit value |> card_attrib)
        ~printer: printer) 

(** [card_test name suit value expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with the string representation of [create_card suit value]. *)
let card_test 
    (name : string) (suit : suit) (value : value) 
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal expected_output 
        (string_of_card_test(create_card suit value)) 
        ~printer:(fun x -> x))

(** [cards_sum_test name card_list expected_output] constructs an OUnit test 
    named [name] that asserts the quality of [expected_output] with 
    [add_cards 0 card_list]. *)
let card_sum_test 
    (name : string) (card_list : card list) (expected_output : int) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal expected_output (add_cards 0 card_list) 
        ~printer: string_of_int)

(** [add_card_to_test name num suit value expected_output] constructs an 
    OUnit test named [name] that asserts the quality of [expected_output] with 
    [add_card_to num (create_card [suit] [value]]. *)
let add_card_to_test 
    (name : string) (num: int) (suit : suit) (value : value)
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal expected_output (add_card_to num (create_card suit value)) 
        ~printer: string_of_int)

(** [sum_cards_test name card_list expected_output] constructs an OUnit test 
    named [name] that asserts the quality of [expected_output] with 
    [sum_cards card_list]. *)
let sum_cards_test 
    (name : string) (card_list : card list) (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (sum_cards card_list) 
        ~printer: string_of_int) 

let card_tests =
  [
    card_attribute_test "testing that the suit is properly created" get_suit
      Spades Two Spades (string_of_suit);
    card_attribute_test "testing that the suit is properly created" get_suit
      Diamonds Two Diamonds (string_of_suit);
    card_attribute_test "testing that the suit is properly created" get_suit
      Clubs Two Clubs (string_of_suit);
    card_attribute_test "testing that the suit is properly created" get_suit
      Hearts Two Hearts (string_of_suit);
    card_attribute_test "testing that the value is properly created" get_value
      Clubs Two Two (string_of_value);
    card_attribute_test "testing that the value is properly created" get_value
      Clubs Ten Ten (string_of_value);
    card_attribute_test "testing that the value is properly created face card" 
      get_value Clubs Ace Ace (string_of_value);
    card_test "testing card creation for non-face cards" Clubs Two 
      "2 of clubs";
    card_test "testing card creation for face cards" Diamonds Queen
      "queen of diamonds";
    card_test "testing card creation for ace cards" Spades Ace
      "ace of spades";
    card_sum_test "the sum of one card is the value of that card" 
      (create_card_list [] [(Spades, Two)]) 2;
    card_sum_test "the sum of two non-face cards" 
      (create_card_list [] [(Spades, Two); (Diamonds, Two)]) 4;
    card_sum_test "the sum of face cards" 
      (create_card_list [] [(Spades, Jack); (Diamonds, Queen)]) 20;
    card_sum_test "the sum of a mix of face and non-face cards" 
      (create_card_list [] 
         [(Spades, Jack); (Clubs, Four); (Hearts, Ten); (Diamonds, Queen)]) 34;
    add_card_to_test "adding an int to a non-face card" 5 Hearts Two 7;
    add_card_to_test "adding an int to a face card" 100 Spades King 110;

    (** Testing sum_cards *)
    sum_cards_test "empty hand" [] 0;
    sum_cards_test "one card" [{suit=Clubs; value=Two}] 2;
    sum_cards_test "10 cards" [{suit=Clubs; value=Two};
                               {suit=Clubs; value=Three};
                               {suit=Clubs; value=Four};
                               {suit=Clubs; value=Five};
                               {suit=Clubs; value=Six};
                               {suit=Clubs; value=Seven};
                               {suit=Clubs; value=Eight};
                               {suit=Clubs; value=Nine};
                               {suit=Clubs; value=Ten};
                               {suit=Clubs; value=Jack}] 64;
    sum_cards_test "ace high" 
      [{suit=Clubs; value=Ace}; {suit=Clubs; value=Jack}] 21;
    sum_cards_test "ace low" 
      [{suit=Clubs; value=Ace}; {suit=Clubs; value=Jack}; 
       {suit=Clubs; value=Two}] 13;
    sum_cards_test "one ace high one ace low" 
      [{suit=Clubs; value=Ace}; {suit=Clubs; value=Jack}; 
       {suit=Clubs; value=Ace}] 12;
    sum_cards_test "one ace high two ace low" 
      [{suit=Clubs; value=Ace}; {suit=Clubs; value=Jack}; 
       {suit=Clubs; value=Ace}; {suit=Clubs; value=Ace}] 13;
    sum_cards_test "one ace high two ace low, bust" 
      [{suit=Clubs; value=Ace}; {suit=Clubs; value=Jack}; 
       {suit=Clubs; value=Ten}; {suit=Clubs; value=Ace}; 
       {suit=Clubs; value=Ace}] 23;
  ]

(** [standard_deck_test name deck expected_output] constructs an 
    OUnit test named [name] that asserts the quality of [expected_output] with 
    [deck]. *)
let standard_deck_test 
    (name : string) (deck : card list) (expected_output : card list) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal expected_output deck ~cmp:cmp_set_like_lists 
        ~printer:(pp_list string_of_card))

let deck_tests =
  [
    standard_deck_test "testing that a standard deck was properly created" 
      create_standard_deck 
      (create_card_list [] 
         [ (Clubs, Two); (Clubs, Three); (Clubs, Four); 
           (Clubs, Five); (Clubs, Six); (Clubs, Seven); (Clubs, Eight); 
           (Clubs, Nine); (Clubs, Ten); (Clubs, Jack); (Clubs, Queen); 
           (Clubs, King); (Clubs, Ace); 
           (Spades, Two); (Spades, Three); (Spades, Four); 
           (Spades, Five); (Spades, Six); (Spades, Seven); (Spades, Eight); 
           (Spades, Nine); (Spades, Ten); (Spades, Jack); (Spades, Queen); 
           (Spades, King); (Spades, Ace); 
           (Hearts, Two); (Hearts, Three); (Hearts, Four); 
           (Hearts, Five); (Hearts, Six); (Hearts, Seven); (Hearts, Eight); 
           (Hearts, Nine); (Hearts, Ten); (Hearts, Jack); (Hearts, Queen); 
           (Hearts, King); (Hearts, Ace); 
           (Diamonds, Two); (Diamonds, Three); 
           (Diamonds, Four); (Diamonds, Five); (Diamonds, Six); 
           (Diamonds, Seven); (Diamonds, Eight); (Diamonds, Nine); 
           (Diamonds, Ten); (Diamonds, Jack); (Diamonds, Queen); 
           (Diamonds, King); (Diamonds, Ace)]);
  ]

(** [init_state_test name state expected_output] constructs an OUnit test 
    named [name] that asserts the quality of [expected_output] with 
    [List.length state]. *)
let init_state_test 
    (name : string) (state : 'a list) (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (List.length state) 
        ~printer: string_of_int) 

(** [house_test name state expected_output] constructs an OUnit test 
    named [name] that asserts the quality of [expected_output] with 
    [List.length state]. *)
let house_test 
    (name : string) (state: string) (expected_output : string) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (state) 
        ~printer:(fun x -> x)) 

(** [print_player_int tuple] returns a string representation of [tuple]. *)
let print_player_int (tuple : Player.player * int) : string = 
  ((fst tuple).name ^ ", " ^ string_of_int(snd tuple))    

(** [player_sums_test name player_sums_list expected_output] constructs an
    OUnit test named [name] that asserts the quality of [expected_output] with 
    [player_sums_list]. *)
let player_sums_test 
    (name : string) (player_sums_list : (Player.player * int) list)
    (expected_output : (Player.player * int) list) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (player_sums_list) 
        ~printer:(pp_list print_player_int))

(** [winner_test name state expected_output] constructs an OUnit test 
    named [name] that asserts the quality of [expected_output] with 
    [determine_round_winners state]. *)
let winner_test 
    (name : string) (state : string) (expected_output : string) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (state)  
        ~printer:(fun x -> x)) 

let standard_state = init_game_state [("Austin", "User"); ("Abby", "User"); 
                                      ("Brennan", "User")] 100 "standard"
let player_sums = get_player_sums [] standard_state.players

(** [string_of_hand hand] returns a string represenatation of [hand]. *)
let string_of_hand (hand : card list) : string = 
  pp_list string_of_card_test hand

(** [string_of_player player] returns a string represenatation of [player]. *)
let string_of_player (player : player) : string = 
  "Name: " ^ player.name ^ ", Current Balance = $"  
  ^ string_of_int (player.balance) ^ ", Hand: " ^ string_of_hand player.hand 
  ^ ", Current Bet: " ^ string_of_int player.current_bet ^ "\n"

(** [state_cycle acc] cycles through each of the players in a given state 
    and concatenates their string representation. *)
let rec state_cycle (acc : string) = function
  | [] -> acc
  | h :: t -> state_cycle (acc ^ string_of_player h) t

(** [string_of_state acc] returns a string represenatation of [curr]. *)
let string_of_state (curr : state) : string = 
  string_of_player curr.house ^ state_cycle "" curr.players

(** [create_state_test name state expected_output] constructs an OUnit test 
    named [name] that asserts the quality of [expected_output] with 
    [determine_round_winners state]. *)
let create_state_test 
    (name : string) (state : state) (expected_output : string) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (string_of_state state)  
        ~printer:(fun x -> x))  

(** [betting_test name state expected_output] constructs an OUnit test 
    named [name] that asserts the quality of [expected_output] with 
    [player_list]. *)
let betting_test 
    (name : string) (player_list : player list) 
    (expected_output : player list) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output player_list
        ~printer:(pp_list string_of_player))

(** [valid_easy player] returns whether the player returned from carrying 
    out an easy AI [player]'s s turn is valid according to the criteria of an 
    Easy move outlined in [easy_turn]. *)
let valid_easy (player : player) : bool = 
  sum_cards player.hand >= 15      

(** [easy_ai_test name updated_player expected_output] constructs an OUnit test 
    named [name] that asserts the quality of [expected_output] with 
    [valid_easy updated_player]. *)
let easy_ai_test 
    (name : string) (updated_player : player)
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (updated_player |> valid_easy)
        ~printer:(string_of_bool))

(** [valid_hard player] returns whether the player returned from carrying 
    out an hard AI [player]'s s turn is valid according to the criteria of an 
    Hard move outlined in [hard_turn]. *)
let valid_hard (player : player) : bool = 
  (* TODO: Update with some criteria once Hard AI is implemented. *)
  sum_cards player.hand >= 17        

(** [hard_ai_test name updated_player expected_output] constructs an OUnit test 
    named [name] that asserts the quality of [expected_output] with 
    [valid_hard updated_player]. *)
let hard_ai_test 
    (name : string) 
    (updated_player : player)
    (dealer_card : int)
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (best_move updated_player dealer_card)
        ~printer:(string_of_int))

let p1 = {name= "Brennan"; hand = []; balance =  100; 
          current_bet =  0; diff="User"} 
let p2 = {name= "Austin"; hand = []; balance =  100; 
          current_bet =  0; diff="User"} 
let p3 = {name= "Abby"; hand = []; balance =  100; 
          current_bet =  0; diff="User"} 
let default_house = {name="HOUSE"; hand=[]; balance=max_int; 
                     current_bet=0; diff="User"}

let default_state = {players = [p1; p2; p3]; 
                     house = default_house; game_name = "default"}

let p2_10 = {name="Austin"; hand=[{suit=Diamonds;value=Ten}]; 
             balance=100; current_bet=0; diff="User"}
let house_10 = {name="HOUSE"; hand=[{suit=Hearts;value=Ten}]; 
                balance=max_int; current_bet=0; diff="User"}
let p2_8 = {name="Austin"; hand=[{suit=Spades;value=Eight}]; 
            balance=100; current_bet=0; diff="User"}
let p1_8 = {name="Brennan"; hand=[{suit=Clubs;value=Eight}]; 
            balance=100; current_bet=0; diff="User"}
let p3_busted = {name="Abby"; 
                 hand=[{suit=Diamonds;value=Eight}; 
                       {suit=Diamonds;value=Nine}; 
                       {suit=Diamonds;value=Ten}]; 
                 balance=100; current_bet=0; diff="User"}           
let house_busted = {name="HOUSE"; 
                    hand=[{suit=Clubs;value=Eight}; 
                          {suit=Clubs;value=Nine}; 
                          {suit=Clubs;value=Ten}]; 
                    balance=max_int; current_bet=0; diff="User"} 
let rich_p1 ={name= "Brennan"; hand = [{suit=Clubs;value=Two}]; 
              balance = 3110; current_bet = 1000; diff="User"} 

let house_win_state = {players = [p1; p2; p3]; house = house_10; 
                       game_name = "default"}
let house_tie_state = {players = [p1; p2_10; p3]; house = house_10; 
                       game_name = "default"}
let p2_8_win_state = {players = [p1; p2_8; p3]; house = default_house; 
                      game_name = "default"}
let p1_p2_tie_state = {players = [p1_8; p2_8; p3]; house = default_house; 
                       game_name = "default"}
let p1_p2_win_busted = {players = [p1_8; p2_8; p3_busted]; 
                        house = house_busted; 
                        game_name = "default"}
let p2_win_busted_rich = {players = [rich_p1; p2_8; p3_busted]; 
                          house = house_busted; game_name = "default"}

let long_name = {name= "ABCDEFGHIJKLMNOPQRSTUVWXYZ"; 
                 hand = [{suit=Clubs;value=Two}; {suit=Clubs;value=Six}]; 
                 balance = 2110; current_bet = 503; diff="User"}

let long_name_game = {players = [rich_p1; p2_8; p3_busted; long_name]; 
                      house = house_busted; game_name = "default"}    

let large_nums = {name= "ZZZZZZZZZZZZZ"; 
                  hand = [{suit=Clubs;value=Two}; {suit=Clubs;value=Eight}]; 
                  balance = max_int; current_bet = max_int; diff="User"}      

let large_name_num_game = {players = [rich_p1; p2_8; p3_busted; 
                                      long_name; large_nums]; 
                           house = house_busted; game_name = "default"}   

let post_bet_rich_p1 = {name= "Brennan"; hand = [{suit=Clubs;value=Two}]; 
                        balance = 5110; current_bet = 1000; diff="User"} 

let post_bet_long_name = {name= "ABCDEFGHIJKLMNOPQRSTUVWXYZ"; 
                          hand = [{suit=Clubs;value=Two}; 
                                  {suit=Clubs;value=Six}]; 
                          balance = 3116; current_bet = 503; diff="User"}                    

let default_easy = {name= "Brennan"; hand = []; balance =  100; 
                    current_bet =  0; diff="Easy"} 
let valid_easy = {name= "Brennan"; 
                  hand = [{suit=Diamonds;value=Eight}; 
                          {suit=Diamonds;value=Nine}; 
                          {suit=Diamonds;value=Ten}]; 
                  balance =  100; current_bet =  0; diff="User"} 

let default_hard = {name= "Abby"; hand = []; balance =  100; 
                    current_bet =  0; diff="Hard"} 
let valid_hard = {name= "Abby"; 
                  hand = [{suit=Diamonds;value=Ace}; 
                          {suit=Diamonds;value=Seven}]; 
                  balance =  100; current_bet =  0; diff="Hard"} 
let valid_hard6 = {name= "Abby"; 
                   hand = [{suit=Diamonds;value=Ace}; 
                           {suit=Diamonds;value=Six}]; 
                   balance =  100; current_bet =  0; diff="Hard"} 
let valid_hard8 = {name= "Abby"; 
                   hand = [{suit=Diamonds;value=Ace}; 
                           {suit=Diamonds;value=Eight}]; 
                   balance =  100; current_bet =  0; diff="Hard"} 

let state_tests =
  [
    init_state_test "testing that init_game_state creates the proper size 
       player list" standard_state.players 3;
    house_test "testing that init_game_state creates the proper size 
       house list" standard_state.house.name "HOUSE";
    init_state_test "testing that init_game_state works as intended" 
      player_sums 3;

    player_sums_test "testing standard player sums" 
      (get_player_sums [] standard_state.players) [(p1,0);(p3,0);(p2,0)];
    player_sums_test "testing default player sums" 
      (get_player_sums [] default_state.players) [(p3,0);(p2,0);(p1,0)];
    player_sums_test "testing house_win_state player sums" 
      (get_player_sums [] house_win_state.players) [(p3,0);(p2,0);(p1,0)];  

    player_sums_test "testing winners_list" 
      (winners_list player_sums) [(p1,0);(p3,0);(p2,0)];
    winner_test "testing get_winner of standard state" (get_winner player_sums) 
      ("Brennan, Abby and Austin are tied with a score of 0, " ^
       "so they won this round. Congrats!\n");  

    create_state_test "testing default state creation" (default_state) 
      ("Name: HOUSE, Current Balance = $4611686018427387903, " ^
       "Hand: [], Current Bet: 0
Name: Brennan, Current Balance = $100, Hand: [], Current Bet: 0
Name: Austin, Current Balance = $100, Hand: [], Current Bet: 0
Name: Abby, Current Balance = $100, Hand: [], Current Bet: 0\n");

    create_state_test "testing house_win_state creation" (house_win_state) 
      ("Name: HOUSE, Current Balance = $4611686018427387903, " ^
       "Hand: [10 of hearts], Current Bet: 0
Name: Brennan, Current Balance = $100, Hand: [], Current Bet: 0
Name: Austin, Current Balance = $100, Hand: [], Current Bet: 0
Name: Abby, Current Balance = $100, Hand: [], Current Bet: 0\n");

    create_state_test "testing house_tie_state creation" (house_tie_state) 
      ("Name: HOUSE, Current Balance = $4611686018427387903," ^ 
       " Hand: [10 of hearts], Current Bet: 0
Name: Brennan, Current Balance = $100, Hand: [], Current Bet: 0
Name: Austin, Current Balance = $100, Hand: [10 of diamonds], Current Bet: 0
Name: Abby, Current Balance = $100, Hand: [], Current Bet: 0\n");

    create_state_test "testing p2_8_win_state creation" (p2_8_win_state) 
      ("Name: HOUSE, Current Balance = $4611686018427387903, Hand: []," ^
       " Current Bet: 0
Name: Brennan, Current Balance = $100, Hand: [], Current Bet: 0
Name: Austin, Current Balance = $100, Hand: [8 of spades], Current Bet: 0
Name: Abby, Current Balance = $100, Hand: [], Current Bet: 0\n");

    create_state_test "testing p1_p2_tie_state creation" (p1_p2_tie_state) 
      ("Name: HOUSE, Current Balance = $4611686018427387903, " ^
       "Hand: [], Current Bet: 0
Name: Brennan, Current Balance = $100, Hand: [8 of clubs], Current Bet: 0
Name: Austin, Current Balance = $100, Hand: [8 of spades], Current Bet: 0
Name: Abby, Current Balance = $100, Hand: [], Current Bet: 0\n");

    create_state_test "testing p1_p2_win_busted creation" (p1_p2_win_busted) 
      ("Name: HOUSE, Current Balance = $4611686018427387903, Hand:" ^ 
       " [8 of clubs; 9 of clubs; 10 of clubs], Current Bet: 0
Name: Brennan, Current Balance = $100, Hand: [8 of clubs], Current Bet: 0
Name: Austin, Current Balance = $100, Hand: [8 of spades], Current Bet: 0
Name: Abby, Current Balance = $100, Hand: [8 of diamonds; 9 of" ^ 
       " diamonds; 10 of diamonds], Current Bet: 0\n");

    create_state_test "testing p2_win_busted_rich creation" (p2_win_busted_rich) 
      ("Name: HOUSE, Current Balance = $4611686018427387903, Hand: " ^ 
       "[8 of clubs; 9 of clubs; 10 of clubs], Current Bet: 0
Name: Brennan, Current Balance = $3110, Hand: [2 of clubs], Current Bet: 1000
Name: Austin, Current Balance = $100, Hand: [8 of spades], Current Bet: 0
Name: Abby, Current Balance = $100, Hand: [8 of diamonds; 9 " ^
       "of diamonds; 10 of diamonds], Current Bet: 0\n");

    create_state_test "testing long_name_game creation" (long_name_game) 
      ("Name: HOUSE, Current Balance = $4611686018427387903," ^
       " Hand: [8 of clubs; 9 of clubs; 10 of clubs], Current Bet: 0
Name: Brennan, Current Balance = $3110, Hand: [2 of clubs], Current Bet: 1000
Name: Austin, Current Balance = $100, Hand: [8 of spades], Current Bet: 0
Name: Abby, Current Balance = $100, Hand: [8 of diamonds;" ^
       " 9 of diamonds; 10 of diamonds], Current Bet: 0
Name: ABCDEFGHIJKLMNOPQRSTUVWXYZ, Current Balance = $2110, " ^
       "Hand: [2 of clubs; 6 of clubs], Current Bet: 503\n");

    create_state_test 
      "testing large_name_num_game creation" (large_name_num_game) 
      ("Name: HOUSE, Current Balance = $4611686018427387903, " ^
       "Hand: [8 of clubs; 9 of clubs; 10 of clubs], Current Bet: 0
Name: Brennan, Current Balance = $3110, Hand: [2 of clubs], Current Bet: 1000
Name: Austin, Current Balance = $100, Hand: [8 of spades], Current Bet: 0
Name: Abby, Current Balance = $100, Hand: [8 of diamonds; 9 of " ^
       "diamonds; 10 of diamonds], Current Bet: 0
Name: ABCDEFGHIJKLMNOPQRSTUVWXYZ, Current Balance = $2110, " ^
       "Hand: [2 of clubs; 6 of clubs], Current Bet: 503
Name: ZZZZZZZZZZZZZ, Current Balance = $4611686018427387903," ^
       " Hand: [2 of clubs; 8 of clubs], Current Bet: 4611686018427387903\n");

    winner_test "testing determine_round_winner with all tied with house" 
      (determine_round_winners default_state) 
      ("HOUSE, Brennan, Austin and Abby are tied with " ^
       "a score of 0, so nobody won this round. Tough luck.\n");
    winner_test "testing determine_game_winners with all tied with house" 
      (determine_game_winners default_state) 
      ("Brennan, Austin and Abby are tied with a balance of 100, " ^
       "so nobody won this game. Tough luck.\n");
    winner_test "testing determine_round_winner with house solo dub" 
      (determine_round_winners house_win_state)       
      "\nThe House won this round with a score of 10. Tough luck. \n"; 
    winner_test "testing determine_game_winners with house solo dub" 
      (determine_game_winners house_win_state)       
      ("Brennan, Austin and Abby are tied with a balance of " ^
       "100, so nobody won this game. Tough luck.\n"); 
    winner_test "testing determine_round_winner with one tied with house" 
      (determine_round_winners house_tie_state) 
      ("HOUSE and Austin are tied with a score of 10," ^
       " so nobody won this round. Tough luck.\n");
    winner_test "testing determine_game_winners with one tied with house" 
      (determine_game_winners house_tie_state) 
      ("Brennan, Austin and Abby are tied with a " ^
       "balance of 100, so nobody won this game. Tough luck.\n"); 
    winner_test "testing determine_round_winner with non-house winner" 
      (determine_round_winners p2_8_win_state) 
      "\nAustin won this round with a score of 8. Congrats!\n";
    winner_test "testing determine_game_winners with non-house winner" 
      (determine_game_winners p2_8_win_state) 
      ("Brennan, Austin and Abby are tied with a balance of" ^
       " 100, so nobody won this game. Tough luck.\n");
    winner_test "testing determine_round_winner with non-house winners" 
      (determine_round_winners p1_p2_tie_state) 
      ("Brennan and Austin are tied with a score of 8," ^
       " so they won this round. Congrats!\n");  
    winner_test "testing determine_game_winners with non-house winners" 
      (determine_game_winners p1_p2_tie_state) 
      ("Brennan, Austin and Abby are tied with a balance of 100," ^
       " so nobody won this game. Tough luck.\n");
    winner_test 
      "testing determine_round_winner with non-house winners and busted players" 
      (determine_round_winners p1_p2_win_busted) 
      ("Brennan and Austin are tied with a score of 8," ^
       " so they won this round. Congrats!\n"); 
    winner_test 
      "testing determine_game_winner with non-house winners and busted players" 
      (determine_game_winners p1_p2_win_busted) 
      ("Brennan, Austin and Abby are tied with a balance of" ^
       " 100, so nobody won this game. Tough luck.\n");  
    winner_test 
      "testing determine_round_winners with non-house winners and busted players
      and one rich player" 
      (determine_round_winners p2_win_busted_rich) 
      "\nAustin won this round with a score of 8. Congrats!\n"; 
    winner_test 
      "testing determine_game_winner with non-house winners and busted players
      and one rich player" 
      (determine_game_winners p2_win_busted_rich) 
      "\nBrennan won this game with a balance of $3110. Congrats!\n\n";
    winner_test 
      "testing determine_round_winners with a player with a long name" 
      (determine_round_winners long_name_game) 
      ("Austin and ABCDEFGHIJKLMNOPQRSTUVWXYZ are tied with" ^
       " a score of 8, so they won this round. Congrats!\n"); 
    winner_test 
      "testing determine_game_winner with a player with a long name" 
      (determine_game_winners long_name_game) 
      "\nBrennan won this game with a balance of $3110. Congrats!\n\n";
    winner_test 
      "testing determine_round_winners with a player with a large sum" 
      (determine_round_winners large_name_num_game) 
      "\nZZZZZZZZZZZZZ won this round with a score of 10. Congrats!\n";
    winner_test 
      "testing determine_game_winner with a player with a large sum" 
      (determine_game_winners large_name_num_game) 
      ("\nZZZZZZZZZZZZZ won this game with a balance" ^
       " of $4611686018427387903. Congrats!\n\n");

    betting_test "testing betting with default_state" 
      (determine_balances default_state) [p3; p2; p1];
    betting_test "testing betting with house_win_state" 
      (determine_balances house_win_state) [p3; p2; p1];  
    betting_test "testing betting with house_tie_state" 
      (determine_balances house_tie_state) [p3; p2_10; p1]; 
    betting_test "testing betting with p2_8_win_state" 
      (determine_balances p2_8_win_state) [p3; p2_8; p1]; 
    betting_test "testing betting with p1_p2_tie_state" 
      (determine_balances p1_p2_tie_state) [p3; p2_8; p1_8]; 
    betting_test "testing betting with p1_p2_win_busted" 
      (determine_balances p1_p2_win_busted) [p3_busted; p2_8; p1_8]; 
    betting_test "testing betting with p2_win_busted_rich" 
      (determine_balances p2_win_busted_rich)
      [p3_busted; p2_8; post_bet_rich_p1]; 
    betting_test "testing betting with long_name_game" 
      (determine_balances long_name_game) 
      [post_bet_long_name; p3_busted; p2_8; post_bet_rich_p1]; 

    (* easy_ai_test "testing default easy AI" 
       (easy_turn default_easy default_state) true;  
       easy_ai_test "testing busted easy AI" 
       (easy_turn valid_easy default_state) true;   *)

    hard_ai_test "testing 7 and ace with 1"
      valid_hard 1 1;
    hard_ai_test "testing 7 and ace with 2"
      valid_hard 2 0;
    hard_ai_test "testing 7 and ace with 3"
      valid_hard 3 1;
    hard_ai_test "testing 7 and ace with 4"
      valid_hard 4 1;
    hard_ai_test "testing 7 and ace with 5"
      valid_hard 5 1;
    hard_ai_test "testing 7 and ace with 6"
      valid_hard 6 1;
    hard_ai_test "testing 7 and ace with 7"
      valid_hard 7 0;
    hard_ai_test "testing 7 and ace with 8"
      valid_hard 8 0;
    hard_ai_test "testing 7 and ace with 9"
      valid_hard 9 1;
    hard_ai_test "testing 7 and ace with 10"
      valid_hard 10 1;
    hard_ai_test "testing 6 and ace with 10"
      valid_hard6 10 1;
    hard_ai_test "testing 8 and ace with 10"
      valid_hard8 10 0;


    (** NOTE: These tests were usesd to ensure that our leaderboards
        were formatted correctly. *)

    (* winner_test 
       "testing determine_game_winner with non-house winners and busted players
       and one rich player" 
       (print_round_leaderboard long_name_game) 
       "\nBrennan won this game with a balance of $3110. Congrats!\n\n";  *)

    (* winner_test 
       "testing determine_game_winner with non-house winners and busted players
       and one rich player" 
       (print_game_leaderboard long_name_game 
         [long_name_game; long_name_game]) ""; *)

    (* winner_test 
       "testing determine_game_winner with non-house winners and busted players
       and one rich player" 
       (print_game_leaderboard p2_win_busted_rich 
         [p1_p2_tie_state; p1_p2_win_busted; p2_win_busted_rich]) ""; *)
  ]

let tests =
  "test suite for Blackjack"  >::: List.flatten [
    card_tests;
    deck_tests;
    state_tests;
  ]

let _ = run_test_tt_main tests
