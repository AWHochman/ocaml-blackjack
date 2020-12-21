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

(** [create_card suit value] creates a card record with the given 
    suit and value. *)
val create_card : suit -> value -> card

(** [tuple_to_card tup] converts a suit * value tuple into a card. *)
val tuple_to_card : (suit * value) -> card

(** [create_card_list acc lst] returns a list of cards corresponding to a list
    of suit * value tuples [lst]. *)
val create_card_list : (card list) -> ((suit * value) list) -> card list

(** [get_suit c] returns the suit of [c]. *)
val get_suit : card -> suit

(** [get_value c] returns the value of [c]. *)
val get_value : card -> value

(** [string_of_suit suit] returns a string representation of [suit]. *)
val string_of_suit : suit -> string

(** [string_of_value value] returns a string representation of [value]. *)
val string_of_value : value -> string

(** [int_of_value value] returns an int representation of [value]. *)
val int_of_value : value -> int

(** [string_of_card c] returns a string representation of [c]. *)
val string_of_card_test : card -> string

(* [add_cards acc lst] returns the sum of the values of the cards in [lst]. *)
val add_cards : int -> (card list) -> int 

(** [add_card_to num c] adds the value of [c] to [num]. *)
val add_card_to : int -> card -> int

(** [find_rem_ace cards acc] returns a [cards] but with 1 ace removed
    if there are any in [cards].*)
val find_rem_ace : card list -> card list -> card list

(** [sum_cards_strict cards acc] returns the sum of a list of cards where 
    aces are counted strictly as 1*)
val sum_cards_strict : card list -> int -> int

(** [sum_cards cards] returns the sum of a list of cards. Aces are counted as 
    either 11 or 1 in manner which gets the player closest to 21 without 
    putting them over 21 *)
val sum_cards : card list -> int

(** [string_of_card] returns a string representation of a card *)
val string_of_card : card -> string

(** [print_cards cards] prints each line of each card in [cards].*) 
val print_cards : card list -> unit

(** [print_hand] prints a list of cards *)
val print_hand : card list -> unit