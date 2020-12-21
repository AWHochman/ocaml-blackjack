open Card

(** [combine_suit_and_values curr suit value_list] creates and combines tuples
    of the form ([suit], value) for each value in [value_list]. *) 
let rec combine_suit_and_values (curr : (suit * value) list) 
    (suit : suit) (value_list : value list) : ((suit * value) list) = 
  match value_list with 
  | [] -> curr
  | h :: t -> combine_suit_and_values ((suit, h) :: curr) suit t

(** [standard_deck_maker acc suit_list value_list] creates a deck consisting
    of the cross product of each element of [suit_list] and [value_list]. *) 
let rec standard_deck_maker acc suit_list value_list = 
  match suit_list with 
  | [] -> acc
  | h :: t -> 
    standard_deck_maker (combine_suit_and_values acc h value_list) t value_list

(** [create_standard_deck] creates a standard 52 card deck. *)
let create_standard_deck : card list = 
  let suit_list = [Clubs; Spades; Hearts; Diamonds] in 
  let value_list = [Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; 
                    Jack; Queen; King; Ace] in
  standard_deck_maker [] suit_list value_list |> create_card_list []

(** [get_card] returns a random card from a standard 52 card deck *)
let get_card a = 
  let deck = create_standard_deck in
  let deck_array = Array.of_list deck in
  Random.self_init ();
  let number = Random.int 52 in
  Array.get deck_array number