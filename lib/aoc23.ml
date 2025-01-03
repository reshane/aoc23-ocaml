
let file_lines filename: string list = 
    let lines = ref [] in
    let ic = open_in filename in
    try 
        while true; do
            lines := input_line ic :: !lines
        done; !lines
    with End_of_file -> 
        close_in_noerr ic;
        List.rev !lines ;;

let parse_int_of_char c = 
    let i = int_of_char c - int_of_char '0' in
    if i < 10 then Some i else None;;

let is_numeric (d: string): bool = 
    match int_of_string_opt d with
    | Some _ -> true
    | None -> false

let explode s = List.init (String.length s) (String.get s)

module Day1 = struct

    let rec tokenize line: int list =
        match line with
            | d :: rest -> (
                let digit = parse_int_of_char d in
                    match digit with
                    | Some dig -> dig :: (tokenize rest)
                    | None -> (tokenize rest)
            )
            | [] -> []

    let digit_prefix (line: string): int option =
        match line with
        | line when ((String.length line) > 0) && (is_numeric (String.sub line 0 1)) -> (int_of_string_opt (String.sub line 0 1))
        | line when ((String.length line) > 2) && (String.sub line 0 3) = "one" -> Some 1
        | line when ((String.length line) > 2) && (String.sub line 0 3) = "two" -> Some 2
        | line when ((String.length line) > 4) && (String.sub line 0 5) = "three" -> Some 3
        | line when ((String.length line) > 3) && (String.sub line 0 4) = "four" -> Some 4
        | line when ((String.length line) > 3) && (String.sub line 0 4) = "five" -> Some 5
        | line when ((String.length line) > 2) && (String.sub line 0 3) = "six" -> Some 6
        | line when ((String.length line) > 4) && (String.sub line 0 5) = "seven" -> Some 7
        | line when ((String.length line) > 4) && (String.sub line 0 5) = "eight" -> Some 8
        | line when ((String.length line) > 3) && (String.sub line 0 4) = "nine" -> Some 9
        | _ -> None

    let rec tokenize_2 (line: string): int list =
        match line with
        | line when (String.length line) = 0 -> []
        | line ->
            match digit_prefix line with
                | Some digit -> (digit :: (tokenize_2 (String.sub line 1 ((String.length line) - 1))))
                | None -> tokenize_2 (String.sub line 1 ((String.length line) - 1))

    let solve_line_2 (line: string): int =
        let toks = (tokenize_2 line) in
            (*print_string (Int.to_string (List.hd toks));
            print_string " ";
            print_endline (Int.to_string (List.hd (List.rev(toks))));*)
            let str_total = ((Int.to_string (List.hd toks)) ^ (Int.to_string (List.hd (List.rev toks)))) in
            (*print_endline (line ^ ": " ^ str_total);*)
            int_of_string str_total

    let solve_line line: int =
        let toks = (tokenize line) in
            (*print_string (Int.to_string (List.hd toks));
            print_string " ";
            print_endline (Int.to_string (List.hd (List.rev(toks))));*)
            int_of_string ((Int.to_string (List.hd toks)) ^ (Int.to_string (List.hd (List.rev toks))))


    let solve_p1 (lines: string list): int = 
        lines
            (*|> List.map (fun x -> print_endline x; x)*)
            |> List.map explode
            |> List.map solve_line
            (*|> List.map (fun x -> print_endline (Int.to_string x); x)*)
            |> List.fold_left (fun a b -> a + b) 0

    let solve_p2 (lines: string list): int = 
        lines
            (*|> List.map (fun x -> print_endline x; x)*)
            |> List.map solve_line_2
            (*|> List.map (fun x -> print_endline (Int.to_string x); x)*)
            |> List.fold_left (fun a b -> a + b) 0

    let solve () =
        let test_lines = (file_lines "d1s") in
            assert ((solve_p1 test_lines) = 142);
        let lines = (file_lines "d1i") in
            print_endline ("Part 1: " ^ (Int.to_string (solve_p1 lines)));
        let test_lines = (file_lines "d1sb") in
            let total = (solve_p2 test_lines) in
            (*print_endline (Int.to_string total);*)
            assert (total = 281);
        let lines = (file_lines "d1i") in
            print_endline ("Part 2: " ^ (Int.to_string (solve_p2 lines)))
end

module Day2 = struct
    type game = {
        red: int;
        green: int;
        blue: int;
        id: int;
    }

    (*let split_space (line: string): string list =
        String.split_on_char ' ' line*)
    (* 
       Need to split on : to get the game id first
       Then need to split on the ; to get each round
       Then need to split on the , to get the individual components
    *)
    let parse_round (s: string) =
        (*Printf.printf "parsing: %s\n" s;*)
        s
            |> String.split_on_char ','
            |> List.fold_left (fun (r,g,b) x -> 
                let parts = String.split_on_char ' ' x 
                    |> List.filter (fun x -> String.length x > 0)
                    |> List.map (fun x -> String.trim x)
                in
                (*print_endline ("[" ^ (List.hd parts) ^ "]");*)
                let ct = int_of_string (List.hd parts) in
                let component = List.hd (List.rev parts) in
                    if String.starts_with ~prefix:"red" component then
                        if r < ct then (ct,g,b) else (r,g,b)
                    else if String.starts_with ~prefix:"green" component then
                        if g < ct then (r,ct,b) else (r,g,b)
                    else if String.starts_with ~prefix:"blue" component then
                        if b < ct then (r,g,ct) else (r,g,b)
                    else (r,g,b)
            ) (0,0,0)

    let game_from_string (line: string): game =
        (*Printf.printf "%s\n" line;*)
        let (id, game_str) = 
            let split = String.split_on_char ':' line in 
            (*print_endline (List.hd (List.rev split));*)
            (*print_endline (List.hd (List.rev (String.split_on_char ' ' (List.hd split))));*)
            (
                List.hd (List.rev (String.split_on_char ' ' (List.hd split))),
                List.hd (List.rev split)
            ) in
        let (r,g,b) = game_str 
            |> String.split_on_char ';'
            (*|> List.map (fun x -> Printf.printf "Round: %s\n" x; x)*)
            |> List.map (fun x -> parse_round x)
            |> List.fold_left (fun (r,g,b) (rx,gx,bx) ->
                let rn = if rx > r then rx else r in
                let gn = if gx > g then gx else g in
                let bn = if bx > b then bx else b in
                (rn, gn, bn)
            ) (0, 0, 0)
        in 
        (*print_endline "parsed rounds";*)
        let g = {
            red = r;
            green = g;
            blue = b;
            id = int_of_string id;
        } in g
    (*
    let print_game (g: game): unit =
        Printf.fprintf stdout
            "Game %d: {\n  red: %d,\n  green: %d,\n  blue: %d,\n}\n"
            g.id g.red g.green g.blue
    *)

    let solve_p1 (lines: string list): int =
        lines
            |> List.map game_from_string 
            (*|> List.map (fun x -> print_game (x); x)*)
            |> List.filter_map (fun g -> if g.red <= 12 && g.green <= 13 && g.blue <= 14 then Some g else None)
            (*|> List.map (fun x -> print_game (x); x)*)
            |> List.fold_left (fun a b -> a + b.id) 0

    let solve_p2 (lines: string list): int =
        lines
            |> List.map game_from_string 
            (*|> List.map (fun x -> print_game (x); x)*)
            |> List.map (fun g -> g.red * g.green * g.blue)
            (*|> List.map (fun x -> print_game (x); x)*)
            |> List.fold_left (fun a b -> a + b) 0

    let solve () =
        let test_lines = file_lines "d2s" in
        assert (8 = solve_p1 test_lines);
        let lines = file_lines "d2i" in
        Printf.printf "Part 1: %d\n" (solve_p1 lines);
        let test_lines = file_lines "d2s" in
        assert (2286 = solve_p2 test_lines);
        let lines = file_lines "d2i" in
        Printf.printf "Part 2: %d\n" (solve_p2 lines)
end
