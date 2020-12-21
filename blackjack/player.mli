open Card
open Deck

(** The type representing a player. *)
type player = {name: string; hand: card list; 
               balance: int; current_bet: int; diff: string}

(** [create_player name hand] creates a player with [name] and [hand]. *)
val create_player : string -> card list -> player

(** [get_hand player] is the hand of [player]. *)
val get_hand : player -> card list

(** [get_sum player] is the sum of [player]. *)
val get_sum : player -> int

(** [get_balance player] is the balance of [player]. *)
val get_balance : player -> int

(** [update_balance] updates a player's balance at the end of a round according 
    to how they did in comparison to the house *)
val update_balance : player -> int -> player

(** [update_player_bet player] returns a new player record with the same 
    fields as [player] except for the sum field which is [new_sum] *)
val update_player_bet : player -> int -> player

(** [initialize_hand player] initializes the hand of [player] by "drawing"
    two cards for them. *)
val initialize_hand : player -> player

(** [hit player] carries out the functionality of hit by updating 
    [player]. *)
val hit : player -> player

(** [house_turn player] takes the houses turn according to house rules. *)
val house_turn : player -> player