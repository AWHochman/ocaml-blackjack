open Player 
open Card 
open Deck 

type state = {players: player list; house: player; game_name: string}

(** [init_game_state player_names] returns a game state with as many players
    as are in [player_names] plus the house *)
val init_game_state : (string * string) list -> int -> string -> state

(** [player_turn] returns a player with their hand updated based on how 
    many times they hit *)
val player_turn : player -> state -> player

val easy_turn : player -> state -> player

(** [play_turns] returns a player list with each player's hand updated based
    on how many times they decided to hit *)
val play_turns : state -> player list -> player list

(** [deal_cards] returns a player list where each player's hand is updated 
    to contain two cards randomly selected from the deck *)
val deal_cards : state -> player list -> player list

(** [place_bets] returns a players list where every player's bets for the 
    current round have been placed *)
val place_bets : state -> player list -> player list

(** [determine_balances] returns a players list with updated balances based 
    on how every player did against the house *)
val determine_balances : state -> player list

(** [start_round] starts a new round of blackjack and returns the state once 
    the game is finished *)
val start_round : state -> state

(** [winners_list lst] returns a list of all of the winners of a round 
    represented by [lst] and their scores. *)
val winners_list : (player * int) list -> (player * int) list

(** [output_multiple_winners winners_list] returns a string that contains the 
    names of all of the winners separated by a semicolon. *)
val output_multiple_winners : (player * int) list -> string -> bool -> string

(** [house_win player_sums] determines whether the house won *)
val house_win : (Player.player * int) list -> bool

(** [get_winner player_sums] determines the winner(s) of a list of players 
    and their scores [player_sums]. *)
val get_winner : (player * int) list -> string

(** [output_multiple_winners_game winners_list balance] prints the 
    winners within [winners_list] and their [balance]. *)
val output_multiple_winners_game : (player * int) list -> string -> string

(** [get_winner player_money] determines the winner(s) of a list of players 
    and their balances [player_sums]. *)
val get_winner_game : (player * int) list -> string

(** [get_player_sums acc players] returns a dictionary [acc] where the key 
    is each player's name in [players] and the associated value is their 
    score at the end of the round. *)
val get_player_sums : (player * int) list -> player list -> (player * int) list

(** [get_player_money acc players] returns a dictionary [acc] where the key 
    is each player's name in [players] and the associated value is their 
    balance at the end of the round. *)
val get_player_money : 
  (player * int) list -> player list -> (player * int) list

(** [compare_players x y] compares [x] and [y] based on their scores. *)
val compare_players : 'a * int -> 'b * int -> int 

(** [determine_round_winners] determines the winner(s) of round [curr]. *)
val determine_round_winners : state -> string 

(** [determine_game_winners] determines the winner(s) of round [curr]. *)
val determine_game_winners : state -> string

(** [print_round_leaderboard] prints the leaderboard at the end of a round. *)
val print_round_leaderboard : state -> string 

(** [print_game_leaderboard] prints the leaderboard at the end of a game. *)
val print_game_leaderboard : state -> state list -> string 