type suit = 
  | Clubs
  | Spades
  | Hearts
  | Diamonds

type value = 
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace

type card = {suit: suit; value: value}

(** [create_card suit value] creates a card record with suit [suit] and 
     value [value]. *)
let create_card suit value = 
  {suit: suit; value: value}

(** [tuple_to_card tup] converts a suit * value tuple [tup] into a card. *)
let tuple_to_card (tup : suit * value) : card = 
  match tup with 
  | (x, y) -> {suit=x; value=y}

(** [create_card_list acc lst] returns a list of cards corresponding to a list
    of suit * value tuples [lst]. *)
let rec create_card_list (acc : card list) 
    (lst : (suit * value) list) : card list = 
  match lst with 
  | [] -> acc
  | h :: t -> create_card_list (tuple_to_card h :: acc) t

(** [get_suit c] returns the suit of [c]. *)
let get_suit (c : card) : suit = 
  c.suit

(** [get_value c] returns the value of [c]. *)
let get_value (c : card) : value = 
  c.value

(** [string_of_suit suit] returns a string representation of [suit]. *)
let string_of_suit (suit : suit) = 
  match suit with 
  | Clubs -> "clubs"
  | Spades -> "spades"
  | Hearts -> "hearts"
  | Diamonds -> "diamonds"

(** [string_of_value value] returns a string representation of [value]. *)
let string_of_value (value : value) = 
  match value with 
  | Two -> "2"
  | Three -> "3"
  | Four -> "4"
  | Five -> "5"
  | Six -> "6"
  | Seven -> "7"
  | Eight -> "8"
  | Nine -> "9"
  | Ten -> "10"
  | Jack -> "jack"
  | Queen -> "queen"
  | King -> "king"
  | Ace -> "ace"

(** [int_of_value value] returns an int representation of [value]. 
    Note: Ace has weird behavior which we will handle in a later module. 
    For now, the default value of Ace is 1. *)
let int_of_value (value : value) = 
  match value with 
  | Two -> 2
  | Three -> 3
  | Four -> 4
  | Five -> 5
  | Six -> 6
  | Seven -> 7
  | Eight -> 8
  | Nine -> 9
  | Ten -> 10
  | Jack -> 10
  | Queen -> 10
  | King -> 10
  | Ace -> 1

(** [string_of_card_test c] returns a string representation of [c]. *)
let string_of_card_test (c : card) = 
  (get_value c |> string_of_value) ^ " of " ^ (get_suit c |> string_of_suit)

(* [add_cards acc lst] returns the sum of the values of the cards in [lst]. *)
let rec add_cards (acc : int) (lst : card list) = 
  match lst with 
  | [] -> acc
  | h :: t -> add_cards (acc + (get_value h |> int_of_value)) t 

(** [add_card_to num c] adds the value of [c] to [num]. 
    Note: We would likely keep track of the current sum of a deck and 
    then use this as opposed to the method above. *)
let add_card_to (num : int) (c : card) : int = 
  num + (get_value c |> int_of_value)

(** [find_rem_ace cards acc] returns [cards] but with 1 ace removed
    if there are any in [cards]. *)
let rec find_rem_ace (cards : card list) acc = 
  match cards with 
    [] -> acc 
  | x :: xs -> if x.value = Ace then acc @ xs 
    else find_rem_ace xs (x :: acc)

(** [sum_cards_strict cards acc] returns the sum of a list of cards where 
    aces are counted strictly as 1. *)
let rec sum_cards_strict (cards : card list) acc = 
  match cards with 
    [] -> acc
  | x :: xs -> let new_acc = int_of_value x.value + acc in 
    (sum_cards_strict xs new_acc) 

(** [sum_cards cards] returns the sum of a list of cards. Aces are counted as 
    either 11 or 1 in manner which gets the player closest to 21 without 
    putting them over 21 *)
let sum_cards (cards : card list) = 
  (* adj_cards = cards with one less ace if there is one *)
  let adj_cards = find_rem_ace cards [] in  

  let len_adj = List.length adj_cards in 
  let len_cards = List.length cards in 

  (* If lens are same then there was no ace in list of cards *)
  let sum_cards = sum_cards_strict adj_cards 0 in
  if len_adj = len_cards then sum_cards 
  else begin if (sum_cards + 11 <= 21) then (sum_cards + 11)
    else (sum_cards + 1) end

(** [string_of_card] returns a string representation of a card *)
let string_of_card card = 
  let string_suit = string_of_suit card.suit in 
  let string_value = string_of_value card.value in 
  "[" ^ string_value ^ "," ^ string_suit ^ "]"

(** [print_top_or_bottom] prints the upper / lower borders of a card. *)
let rec print_top_or_bottom = function
  | [] -> print_string ("\n")
  | h :: t -> if h.suit = Hearts || h.suit = Diamonds then 
      ANSITerminal.(print_string [red] "*********  ") 
    else ANSITerminal.(print_string [default] "*********  "); 
    print_top_or_bottom t

(** [clubs_second_line card] takes in a card [cards] and prints the second line 
    of the card with the value followed by the C for clubs, spaced 
    appropriately. *)  
let clubs_second_line = function
  | Two -> ANSITerminal.(print_string [default] "* 2   C *  ")
  | Three -> ANSITerminal.(print_string [default] "* 3   C *  ")
  | Four -> ANSITerminal.(print_string [default] "* 4   C *  ")
  | Five -> ANSITerminal.(print_string [default] "* 5   C *  ")
  | Six -> ANSITerminal.(print_string [default] "* 6   C *  ")
  | Seven -> ANSITerminal.(print_string [default] "* 7   C *  ")
  | Eight -> ANSITerminal.(print_string [default] "* 8   C *  ")
  | Nine -> ANSITerminal.(print_string [default] "* 9   C *  ")
  | Ten -> ANSITerminal.(print_string [default] "* 10  C *  ")
  | Jack -> ANSITerminal.(print_string [default] "* J   C *  ")
  | Queen -> ANSITerminal.(print_string [default] "* Q   C *  ")
  | King -> ANSITerminal.(print_string [default] "* K   C *  ")
  | Ace -> ANSITerminal.(print_string [default] "* A   C *  ")

(** [spades_second_line card] takes in a card [card] and prints the second 
    line of the card with the value followed by the S for spades, 
    spaced appropriately.*) 
let spades_second_line = function
  | Two -> ANSITerminal.(print_string [default] "* 2   S *  ")
  | Three -> ANSITerminal.(print_string [default] "* 3   S *  ")
  | Four -> ANSITerminal.(print_string [default] "* 4   S *  ")
  | Five -> ANSITerminal.(print_string [default] "* 5   S *  ")
  | Six -> ANSITerminal.(print_string [default] "* 6   S *  ")
  | Seven -> ANSITerminal.(print_string [default] "* 7   S *  ")
  | Eight -> ANSITerminal.(print_string [default] "* 8   S *  ")
  | Nine -> ANSITerminal.(print_string [default] "* 9   S *  ")
  | Ten -> ANSITerminal.(print_string [default] "* 10  S *  ")
  | Jack -> ANSITerminal.(print_string [default] "* J   S *  ")
  | Queen -> ANSITerminal.(print_string [default] "* Q   S *  ")
  | King -> ANSITerminal.(print_string [default] "* K   S *  ")
  | Ace -> ANSITerminal.(print_string [default] "* A   S *  ")

(** [hearts_second_line card] takes in a card [card] and prints the second 
    line of the card with the value followed by the H for hearts, 
    spaced appropriately.*) 
let hearts_second_line = function
  | Two -> ANSITerminal.(print_string [red] "* 2   H *  ")
  | Three -> ANSITerminal.(print_string [red] "* 3   H *  ")
  | Four -> ANSITerminal.(print_string [red] "* 4   H *  ")
  | Five -> ANSITerminal.(print_string [red] "* 5   H *  ")
  | Six -> ANSITerminal.(print_string [red] "* 6   H *  ")
  | Seven -> ANSITerminal.(print_string [red] "* 7   H *  ")
  | Eight -> ANSITerminal.(print_string [red] "* 8   H *  ")
  | Nine -> ANSITerminal.(print_string [red] "* 9   H *  ")
  | Ten -> ANSITerminal.(print_string [red] "* 10  H *  ")
  | Jack -> ANSITerminal.(print_string [red] "* J   H *  ")
  | Queen -> ANSITerminal.(print_string [red] "* Q   H *  ")
  | King -> ANSITerminal.(print_string [red] "* K   H *  ")
  | Ace -> ANSITerminal.(print_string [red] "* A   H *  ")

(** [diamonds_second_line card] takes in a card and prints the second line of 
    the card with the value followed by the D for diamonds, 
    spaced appropriately.*) 
let diamonds_second_line = function
  | Two -> ANSITerminal.(print_string [red] "* 2   D *  ")
  | Three -> ANSITerminal.(print_string [red] "* 3   D *  ")
  | Four -> ANSITerminal.(print_string [red] "* 4   D *  ")
  | Five -> ANSITerminal.(print_string [red] "* 5   D *  ")
  | Six -> ANSITerminal.(print_string [red] "* 6   D *  ")
  | Seven -> ANSITerminal.(print_string [red] "* 7   D *  ")
  | Eight -> ANSITerminal.(print_string [red] "* 8   D *  ")
  | Nine -> ANSITerminal.(print_string [red] "* 9   D *  ")
  | Ten -> ANSITerminal.(print_string [red] "* 10  D *  ")
  | Jack -> ANSITerminal.(print_string [red] "* J   D *  ")
  | Queen -> ANSITerminal.(print_string [red] "* Q   D *  ")
  | King -> ANSITerminal.(print_string [red] "* K   D *  ")
  | Ace -> ANSITerminal.(print_string [red] "* A   D *  ")

(** [clubs_fourth_line card] takes in a card and prints the fourth line of the
    card with a C for clubs followed by the value, spaced appropriately.*) 
let clubs_fourth_line = function
  | Two -> ANSITerminal.(print_string [default] "* C   2 *  ")
  | Three -> ANSITerminal.(print_string [default] "* C   3 *  ")
  | Four -> ANSITerminal.(print_string [default] "* C   4 *  ")
  | Five -> ANSITerminal.(print_string [default] "* C   5 *  ")
  | Six -> ANSITerminal.(print_string [default] "* C   6 *  ")
  | Seven -> ANSITerminal.(print_string [default] "* C   7 *  ")
  | Eight -> ANSITerminal.(print_string [default] "* C   8 *  ")
  | Nine -> ANSITerminal.(print_string [default] "* C   9 *  ")
  | Ten -> ANSITerminal.(print_string [default] "* C  10 *  ")
  | Jack -> ANSITerminal.(print_string [default] "* C   J *  ")
  | Queen -> ANSITerminal.(print_string [default] "* C   Q *  ")
  | King -> ANSITerminal.(print_string [default] "* C   K *  ")
  | Ace -> ANSITerminal.(print_string [default] "* C   A *  ")

(** [spades_fourth_line card] takes in a card and prints the fourth line of the
    card with a S for spades followed by the value, spaced appropriately.*) 
let spades_fourth_line = function
  | Two -> ANSITerminal.(print_string [default] "* S   2 *  ")
  | Three -> ANSITerminal.(print_string [default] "* S   3 *  ")
  | Four -> ANSITerminal.(print_string [default] "* S   4 *  ")
  | Five -> ANSITerminal.(print_string [default] "* S   5 *  ")
  | Six -> ANSITerminal.(print_string [default] "* S   6 *  ")
  | Seven -> ANSITerminal.(print_string [default] "* S   7 *  ")
  | Eight -> ANSITerminal.(print_string [default] "* S   8 *  ")
  | Nine -> ANSITerminal.(print_string [default] "* S   9 *  ")
  | Ten -> ANSITerminal.(print_string [default] "* S  10 *  ")
  | Jack -> ANSITerminal.(print_string [default] "* S   J *  ")
  | Queen -> ANSITerminal.(print_string [default] "* S   Q *  ")
  | King -> ANSITerminal.(print_string [default] "* S   K *  ")
  | Ace -> ANSITerminal.(print_string [default] "* S   A *  ")

(** [hearts_fourth_line card] takes in a card and prints the fourth line of the
    card with an H for hearts followed by the value, spaced appropriately.*) 
let hearts_fourth_line = function
  | Two -> ANSITerminal.(print_string [red] "* H   2 *  ")
  | Three -> ANSITerminal.(print_string [red] "* H   3 *  ")
  | Four -> ANSITerminal.(print_string [red] "* H   4 *  ")
  | Five -> ANSITerminal.(print_string [red] "* H   5 *  ")
  | Six -> ANSITerminal.(print_string [red] "* H   6 *  ")
  | Seven -> ANSITerminal.(print_string [red] "* H   7 *  ")
  | Eight -> ANSITerminal.(print_string [red] "* H   8 *  ")
  | Nine -> ANSITerminal.(print_string [red] "* H   9 *  ")
  | Ten -> ANSITerminal.(print_string [red] "* H  10 *  ")
  | Jack -> ANSITerminal.(print_string [red] "* H   J *  ")
  | Queen -> ANSITerminal.(print_string [red] "* H   Q *  ")
  | King -> ANSITerminal.(print_string [red] "* H   K *  ")
  | Ace -> ANSITerminal.(print_string [red] "* H   A *  ")

(** [diamonds_fourth_line card] takes in a card and prints the fourthline of the
    card with a D for diamonds followed by the value, spaced appropriately.*) 
let diamonds_fourth_line = function
  | Two -> ANSITerminal.(print_string [red] "* D   2 *  ")
  | Three -> ANSITerminal.(print_string [red] "* D   3 *  ")
  | Four -> ANSITerminal.(print_string [red] "* D   4 *  ")
  | Five -> ANSITerminal.(print_string [red] "* D   5 *  ")
  | Six -> ANSITerminal.(print_string [red] "* D   6 *  ")
  | Seven -> ANSITerminal.(print_string [red] "* D   7 *  ")
  | Eight -> ANSITerminal.(print_string [red] "* D   8 *  ")
  | Nine -> ANSITerminal.(print_string [red] "* D   9 *  ")
  | Ten -> ANSITerminal.(print_string [red] "* D  10 *  ")
  | Jack -> ANSITerminal.(print_string [red] "* D   J *  ")
  | Queen -> ANSITerminal.(print_string [red] "* D   Q *  ")
  | King -> ANSITerminal.(print_string [red] "* D   K *  ")
  | Ace -> ANSITerminal.(print_string [red] "* D   A *  ")

(** [print_second_line hand] takes in a list of cards and prints the second
    line of each card with the appropriate suit and value.*) 
let rec print_second_line = function
  | [] -> print_string ("\n")
  | h :: t -> begin
      match h.suit with
      | Clubs -> clubs_second_line h.value; print_second_line t
      | Spades -> spades_second_line h.value; print_second_line t
      | Hearts -> hearts_second_line h.value; print_second_line t
      | Diamonds -> diamonds_second_line h.value; print_second_line t
    end

(** [print_third_line hand] takes in a list of cards and prints the third
    line of each card.*) 
let rec print_third_line = function
  | [] -> print_string ("\n")
  | h :: t -> if h.suit = Hearts || h.suit = Diamonds then 
      ANSITerminal.(print_string [red] "*       *  ") 
    else ANSITerminal.(print_string [default] "*       *  "); 
    print_third_line t

(** [print_fourth_line hand] takes in a list of cards and prints the fourth
    line of each card with the appropriate suit and value.*) 
let rec print_fourth_line = function
  | [] -> print_string ("\n")
  | h :: t -> begin
      match h.suit with
      | Clubs -> clubs_fourth_line h.value; print_fourth_line t
      | Spades -> spades_fourth_line h.value; print_fourth_line t
      | Hearts -> hearts_fourth_line h.value; print_fourth_line t
      | Diamonds -> diamonds_fourth_line h.value; print_fourth_line t
    end

(** [print_cards cards] prints each line of each card in [cards].*) 
let print_cards cards =
  print_top_or_bottom cards;
  print_second_line cards;
  print_third_line cards;
  print_fourth_line cards;
  print_top_or_bottom cards

(** [print_hand] prints a list of cards *)
let print_hand cards = 
  print_string "Current Hand:\n";
  print_cards cards;
  let string_sum = string_of_int (sum_cards cards) in
  print_string ("Sum:" ^ string_sum ^ "\n")