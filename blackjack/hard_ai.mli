
(** The type of tuples representing the sum of the player's hand and value of
    the first card in the dealer's hand. *)
module CardTup : sig
  type t
  val compare : int * int -> int * int -> int
end

(** The hashtable that maps CardTups to an integer, either 0 or 1. *)
module CardTupMap : sig
end

(** [decision_table] is the hashtable that maps CardTups to an integer, 
    either 0 or 1. *)
val decision_table : ((int * int), int) Hashtbl.t

(** [stays] is list of CardTups for which the player should stay. *)
val stays : (int * int) list

(** [hits] is the list of CardTups for which the player should hit. *)
val hits : (int * int) list

(** [insert_tup_list] nserts each CardTup in [tup_list] and corresponding value 
    [entry] into the decision table. *)
val insert_tup_list : (int * int) list -> int -> unit

(** [initialize_decision_table] adds all of the values in [hits] and [stays] and their corresponding values 
    to the hashtable. *)
val initialize_decision_table : unit -> unit

(** [best_move player dealer_hand] returns the best move an AI player can make, 
    with 0 representing the decision to stay, and 1 representing the decision 
    to hit.*)
val best_move : Player.player -> int -> int




