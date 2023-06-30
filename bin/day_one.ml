open Base
open Stdio

let main filename = 
    let rec aux current max = function
        | [] -> print_endline (Int.to_string (Int.max current max)) 
        | line :: lines -> 
                if (String.equal "" line) then aux 0 (Int.max current max) lines
                else aux ((Int.of_string line) + current) max lines
    in 
    aux 0 0 (In_channel.input_lines (In_channel.create filename))

let () = match Array.to_list (Sys.get_argv ()) with 
| _ :: filename :: _ -> main filename
| _ -> print_endline "No filename specified."
