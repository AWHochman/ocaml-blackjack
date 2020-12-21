open Card

(** [combine_suit_and_values curr suit value_list] creates and combines tuples
    of the form ([suit], value) for each value in [value_list]. *) 
val combine_suit_and_values : ((suit * value) list) -> suit -> 
  (value list) -> ((suit * value) list)

(** [standard_deck_maker] creates a deck consisting of the cross product 
    of the suits and values provided. *) 
val standard_deck_maker : (suit * value) list -> suit list -> value list 
  -> (suit * value) list

(** [create_standard_deck] creates a standard 52 card deck. *)
val create_standard_deck : card list

(** [get_card] returns a random card from a standard 52 card deck. *)
val get_card : unit -> card