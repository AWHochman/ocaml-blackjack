open Card
open Deck
open Player
open State
open Save
open Hard_ai

(** [play_round state here] carries out all of the functionality of playing a 
    round of Blackjack with associated game information [state]. [here] 
    is used to prevent this functionality from occuring when the 
    file is loaded. *)
let play_round state here = 
  if here |> not then failwith "Should never get here" 
  else start_round state

(** [save_game_qry state] handles allowing a user to decide to save their 
    game. *)
let rec save_game_qry state = 
  print_string "\n\nWould you like to save your game? (y/n) ";
  let save_game_str = read_line () in 
  match save_game_str with 
  | "y" -> save_game state 
  | "n" -> ()
  | _ -> print_string "Please input a valid response\n"; save_game_qry state

(** [repeat_rounds state here] allows a user to play a round and 
    update [state] along the way. 
    [here] is used to prevent this functionality from occuring when the 
    file is loaded.*)
let rec repeat_rounds state past_states here = 
  if here |> not then failwith "repeat_rounds here = not" else 
    print_string "\n\nWould you like to play another round? (y/n) ";
  let str = read_line () in
  match str with 
  | "n" -> 
    save_game_qry state;
    print_string "\n";
    print_string (determine_round_winners state);
    print_string (determine_game_winners state);
    print_string (print_game_leaderboard state past_states);
    print_string "Thanks for playing!\n\n\n"
  | "y" ->  
    print_string "\n\n\n";
    print_string (determine_round_winners state);
    print_string (print_round_leaderboard state);
    let y = play_round state true in
    repeat_rounds (y) (y :: past_states) true
  | other -> print_string "\nPlease type a valid input.\n";
    repeat_rounds state past_states here

(** [prompt_name here] prompts a user to enter their name and handles
    their response. [here] is used to prevent this functionality from 
    occuring when the file is loaded. *)
let prompt_name here =
  if here |> not then failwith "prompt_name here = not" else 
    ANSITerminal.(print_string [red]
                    "\n\nWelcome new player. What will your name be?.");
  print_string "\n";
  print_string  "> ";
  match read_line () with
  | name -> name

(** [prompt_diff here name] prompts a user to enter their difficulty level 
    of an AI player named [name] and handles their response. [here] is used 
    to prevent this functionality from occuring when the file is loaded. *)
let rec prompt_diff here name =
  if here |> not then failwith "prompt_diff here = not" else 
    ANSITerminal.(print_string [red]
                    ("\n\nWhat difficulty will "^ name ^ " be?\n" ^
                     "Enter [\"Easy\"] for Easy or [\"Hard\"] for Hard."));
  print_string "\n";
  print_string  "> ";
  let diff = read_line () in 
  match diff with
  | "Easy" | "easy" -> "Easy"
  | "Hard" | "hard" -> "Hard"
  | _ -> print_string "\nPlease type a valid input.\n";
    prompt_diff here name

(** [prompt_user_or_ai here] prompts a user to enter their name and handles
    their response. It also handles determining the difficulty level of the 
    player if they are an AI player. [here] is used to prevent this 
    functionality from occuring when the file is loaded. *)
let rec prompt_user_or_ai here name =
  if here |> not then failwith "prompt_diff here = not" else 
    ANSITerminal.(print_string [red] (
        "\n\nWill " ^ name ^ 
        " be a user-controlled player or an AI-controlled player?\n" ^ 
        "Enter [\"User\"] for a user-controlled player or " ^ 
        "[\"AI\"] for an AI player."));
  print_string "\n";
  print_string  "> ";
  let player_type = read_line () in 
  match player_type with
  | "user" | "User" -> "User" 
  | "AI" | "ai" | "Ai" -> prompt_diff true name
  | _ -> print_string "\nPlease type a valid input.\n";
    prompt_user_or_ai here name

(** [get_names name_list n] returns [name_and_diff_list] which has the names 
    and associated difficulties of the [n] players. *)
let rec get_names_and_diff (name_and_diff_list : (string * string) list) 
    (n : int) : (string * string) list =
  match n with 
  | 0 -> name_and_diff_list
  | x -> let next_name = prompt_name true in 
    let next_diff = prompt_user_or_ai true next_name in 
    get_names_and_diff ((next_name, next_diff) :: name_and_diff_list) (n-1)

(** [initialize_game here] initializes the game by gathering the number of
    players and returning an initial game state with each of these players' 
    names. [here] is used to prevent this functionality from occuring when the 
    file is loaded.*)
let rec initialize_game here =
  if here |> not then failwith "initialize_game here = not" else
    ANSITerminal.(print_string [red]
                    "\n\nHow many players will there be?.\n");
  ANSITerminal.(print_string [red] 
                  "Please enter a number between 1 and 100 inclusive.");
  print_string "\n";
  print_string  "> "; 
  match read_line () with  
  | number_of_players -> let n = number_of_players |> int_of_string_opt in 
    if n = None || n < Some 0 || n > Some 100 
    then begin
      print_endline "\n";
      print_string "Invalid input. Try again.";
      initialize_game here
    end
    else number_of_players 
         |> int_of_string 
         |> get_names_and_diff [] 
         |> init_game_state

let rules_string = "Welcome to Blackjack!
Your goal is to beat the dealer's hand without going over 21. Face cards are 
worth 10, aces are worth 1 or 11 (whichever is better for your hand). All other 
cards are worth their value. Each player begins with two cards. The players' 
cards are known to all, but only one of the dealer's cards is visible. To hit 
(h) is to ask for another card. To stand (s) is end your turn without 
getting another card. If the total value of your hand goes over 21, you bust,
and the dealer wins. Press control c at any time to exit the game.\n"

(** [print_rules ()] prints the rules string along with correct spacing and 
    prompts the players as to whether or not they are ready to continue. *)
let print_rules () = 
  print_endline "\n";
  print_string rules_string;
  print_endline "\n";
  ANSITerminal.(print_string [red] "Are you ready to play? (y/n)" );
  print_string "\n";
  print_string  "> "

(** [rules here] handles outputting the rules if a player desires them. *)
let rec rules here =
  print_rules ();
  match read_line () with 
  | "y" -> begin
      ANSITerminal.(print_string [red] 
                      "\n\nWhat would you like the name of this game to be?");
      print_string "\n";
      print_string  "> "; 
      let game_name = read_line () in 
      ANSITerminal.(print_string [red] 
                      "\n\nHow much money should each player start with?");
      print_string "\n";
      print_string  "> "; 
      let start_amount = read_line () |> int_of_string in 
      let init_state = initialize_game here start_amount (game_name) in 
      let final_state = start_round init_state in 
      (*repeat_rounds_fake true; *)
      repeat_rounds final_state [final_state] true

    end
  | _ -> begin
      print_endline "\n";
      print_string
        ("That wasn't a yes, so here are the rules" ^
         "again. Try reading them this time. ");
      rules here
    end  

(** [select_game here] returns a state object afting prompting the players
    to choose whether they want to start a new game or load an existing one. If 
    they decide to load an existing game it then prompts them to enter the name 
    of the game they would like to load.  *)
let rec select_game here = 
  ANSITerminal.(print_string [red] 
                  "\n\nWould you like to load an existing game? (y/n)");
  print_string "\n";
  print_string  "> "; 
  match read_line () with 
  | "y" -> 
    ANSITerminal.
      (print_string [red]
         "\nEnter the name of the game file (omit the .json extension):");
    print_string "\n";
    print_string  "> "; 
    let name = read_line () in 
    load_game name 
  | "n" -> 
    print_string "\n\n";
    ANSITerminal.(print_string [red] 
                    "What would you like the name of this game to be?");
    print_string "\n";
    print_string  "> "; 
    let game_name = read_line () in 
    ANSITerminal.(print_string [red] 
                    "\n\nHow much money should each player start with?");
    print_string "\n";
    print_string  "> "; 
    let start_amount = read_line () |> int_of_string in 
    initialize_game here start_amount game_name 
  | _ -> 
    print_endline "\n";
    print_string "Please type a valid input. ";
    select_game here

(** [play_game here] starts the Blackjack game. [here] is used to prevent
    this functionality from occuring when the file is loaded. *)
let rec play_game here =
  print_endline "\n";
  ANSITerminal.(print_string [red] "Would you like to see the rules? (y/n) ");
  print_string "\n";
  print_string  "> "; 
  match read_line () with 
  | "y" -> rules here 
  | "n" -> begin
      let init_state = select_game true in 
      let final_state = start_round init_state in 
      (*repeat_rounds_fake true; *)
      repeat_rounds final_state [final_state] true 
      (* in 
         determine_big_winner final_round *)
    end
  | _ -> begin
      print_endline "\n";
      print_string "Please type a valid input. ";
      play_game here
    end

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  initialize_decision_table ();
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to Blackjack.\n");
  ANSITerminal.(print_string [red] 
                  "Would you like to start a game? (press 'y' for yes)\n");
  print_string  "> ";
  match read_line () with
  | interested -> if interested = "y" then play_game true 
    else begin
      print_string "Why did you even start the game in the first place?\nBye";
      print_endline "\n"
    end

(** Execute the game engine. *)
let () = main ()
