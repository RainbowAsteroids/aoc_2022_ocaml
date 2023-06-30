open Base
open Stdio

exception NeedleMissing
exception UnknownCharacter of char

let sum list =
    let rec aux acc = function
        | [] -> acc
        | x :: xs -> aux (acc + x) xs
    in
    aux 0 list

let split_string str =
    let half = (String.length str) / 2 in
    (String.sub str ~pos:0 ~len:half), (String.sub str ~pos:half ~len:half)

let find_common (str1, str2) =
    let rec aux str = function
        | char :: chars -> 
                if String.contains str char then char
                else aux str chars
        | [] -> raise NeedleMissing 
    in
    aux str1 (String.to_list str2)

let priority_of_char char =
    if Char.is_lowercase char then
        (* lowercase characters start at decimal 97 and end at decimal 122 *)
        (Char.to_int char) - 96
    else if Char.is_uppercase char then
        (* uppercase characters start at decimal 65 and end at decimal 90 *)
        (Char.to_int char) - (64 - 26)
    else
        raise (UnknownCharacter char)

let main filename = 
    print_endline (
    Int.to_string (
    sum (
    List.map (In_channel.input_lines (In_channel.create filename)) 
        ~f:(fun line -> split_string line |> find_common |> priority_of_char)
    )))

let () = match Array.to_list (Sys.get_argv ()) with 
| _ :: filename :: _ -> main filename
| _ -> print_endline "No filename specified."
