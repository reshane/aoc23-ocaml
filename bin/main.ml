open Aoc23

let solve () =
    Printf.printf "Current Day\n"

let () = 
    let days = [|solve; Day1.solve; Day2.solve|] in
    match Array.length Sys.argv with
        | 0 -> Printf.printf "How did you do this\n"
        | 1 -> (
            days.(0) ()
        )
        | _ -> (
            for i = 1 to Array.length Sys.argv - 1 do
                match int_of_string_opt Sys.argv.(i) with
                    | Some day -> (
                        if day < Array.length days then (
                            days.(day) ()
                        ) else 
                            Printf.printf "No implementation for day %s\n" Sys.argv.(i)
                    )
                    | None -> Printf.printf "%s is not a valid day number\n" Sys.argv.(i)
            done
        )
