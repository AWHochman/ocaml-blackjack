open Card
open Deck

(** The type representing a player. *)
type player = {name: string; hand: card list; balance: int; 
               current_bet: int; diff: string}

(** [create_player name hand] creates a player with [name] and [hand]. *)
let create_player name hand =
  {name = name; hand = hand; balance = 0; current_bet = 0; diff = "User"}

(** [get_hand player] is the hand of [player]. *)
let get_hand player =
  player.hand

(** [get_sum player] is the sum of [player]. *)
let get_sum player =
  sum_cards player.hand

(** [get_balance player] is the balance of [player]. *)
let get_balance player =
  player.balance

(** [update_balance] updates a player's balance at the end of a round according 
    to how they did in comparison to the house *)
let update_balance player house_score = 
  let player_score = get_sum player in 
  if player_score > 21 then player 
  (* else if house_score = player_score then 
     let new_bal = player.balance+player.current_bet in 
     {player with balance=new_bal} *)
  else if house_score > 21 || player_score > house_score then 
    let new_bal = player.balance+player.current_bet*2 in 
    {player with balance=new_bal}
  else 
    player

(** [update_player_bet player] returns a new player record with the same 
    fields as [player] except for the sum field which is [new_sum] *)
let update_player_bet player new_sum = 
  {name = player.name; hand = player.hand; balance = player.balance-new_sum; 
   current_bet = new_sum; diff = player.diff}

(** [initialize_hand player] initializes the hand of [player] by "drawing"
    two cards for them. *)
let initialize_hand player = 
  let card1 = get_card () in 
  let card2 = get_card () in 
  let init_hand = [card1; card2] in 
  {name=player.name; hand=init_hand; 
   balance=player.balance; current_bet=player.current_bet; diff = player.diff}

(** [hit player] carries out the functionality of hit by updating 
    [player]. *)
let hit player =
  let new_card = get_card () in 
  let new_hand = new_card :: player.hand in 
  {name=player.name; hand=new_hand; 
   balance=player.balance; current_bet=player.current_bet; diff = player.diff}

(** [house_turn player] takes the houses turn according to 
    the house rules. *)
let house_turn player = 
  if sum_cards player.hand < 17 
  then hit player 
  else player 