open Base
open Stdio

exception ParseFailure of string

let sum list =
    let rec aux acc = function
        | [] -> acc
        | x :: xs -> aux (acc + x) xs
    in
    aux 0 list

type shape =
    | Rock
    | Paper
    | Scissors

type game_result =
    | Win
    | Loss
    | Draw

let score_of_shape = function
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

let score_of_game_result = function
    | Win -> 6
    | Loss -> 0
    | Draw -> 3

let game_result_of_shapes yours other = 
    match (yours, other) with
    | (Rock, Scissors)
    | (Scissors, Paper)
    | (Paper, Rock) -> Win

    | (Rock, Paper) 
    | (Paper, Scissors)
    | (Scissors, Rock) -> Loss

    | (Rock, Rock)
    | (Paper, Paper)
    | (Scissors, Scissors) -> Draw


let score_of_game (other, yours) = 
    (score_of_shape yours) + (score_of_game_result (game_result_of_shapes yours other))

let shapes_of_line = function
    | "A X" -> (Rock, Rock)
    | "A Y" -> (Rock, Paper)
    | "A Z" -> (Rock, Scissors)
    | "B X" -> (Paper, Rock)
    | "B Y" -> (Paper, Paper)
    | "B Z" -> (Paper, Scissors)
    | "C X" -> (Scissors, Rock)
    | "C Y" -> (Scissors, Paper)
    | "C Z" -> (Scissors, Scissors)
    | s -> raise (ParseFailure s)

let main filename = 
    print_endline (
    Int.to_string (
    sum (
    List.map (In_channel.input_lines (In_channel.create filename)) 
        ~f:(fun line -> shapes_of_line line |> score_of_game)
    )))

let () = match Array.to_list (Sys.get_argv ()) with 
| _ :: filename :: _ -> main filename
| _ -> print_endline "No filename specified."
