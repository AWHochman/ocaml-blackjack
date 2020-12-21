open Yojson.Basic.Util
open Player 
open State

(** [player_of_savedp] returns a player object based on the Yojson.Basic.t
    object in [saved_p]. *)
val player_of_savedp : Yojson.Basic.t -> Player.player


(** [load_game name] returns a game state based on the contents from the json
    file whose name is [name]. 

    Precondition: [name] is the name of a valid blackjack json file in the same
    directory, ommitting the ".json" extension.   *)
val load_game : string -> State.state

(** [remove_l_comma str] removes the last character from the string [str]. 
    This is needed to remove the comma at the end of a Json string so that it is 
    properly formatted to work with Yojson. *)
val remove_l_comma : string -> string

(** [json_of_players state] returns a Yojson.Safe.t object based on the players
    in the current game in [state]. *)
val json_of_players : state -> Yojson.Safe.t

(** [save_game state] saves the data in a games state object [state] to 
    a json file with the game's name (also stored in [state]). 
    If a json file with the name exists already it will write to that file
    and otherwise a new json file will be created.  *)
val save_game : state -> unit