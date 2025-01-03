open Aoc23

let is_num (v: string): bool =
    match int_of_string_opt v with
    | Some _ -> true
    | None -> false

let append  xs ys = List.rev_append (List.rev xs) ys

let get_nbors map (x,y): int list =
    (* get top neighbors *)
    let top_left = Hashtbl.find_opt map (x-1,y-1) in
    let top = Hashtbl.find_opt map (x,y-1) in
    let top_right = Hashtbl.find_opt map (x+1,y-1) in
    (*Printf.printf "TL: (%d,%d), T: (%d,%d), TR: (%d,%d)\n" (x-1) (y-1) x (y-1) (x+1) (y-1);*)
    (* Some None Some is two distinct neighbors *)
    (* Some Some None is one distinct neighbor *)
    (* None Some Some is one distinct neighbor *)
    let t_bors = match top with
    | Some nbor -> (*Printf.printf "T: %d" nbor;*) nbor :: []
    | None -> (
        match (top_left, top_right) with
            | (Some l, Some r) -> (*(Printf.printf "Two nbors: %d, %d\n" l r);*) l :: r :: []
            | (Some l, None) -> (*(Printf.printf "TL: %d\n" l);*) l :: []
            | (None, Some r) -> (*(Printf.printf "TR: %d\n" r);*) r :: []
            | (None, None) -> (Printf.printf ""); []
    ) in
    let bottom_left = Hashtbl.find_opt map (x-1,y+1) in
    let bottom = Hashtbl.find_opt map (x,y+1) in
    let bottom_right = Hashtbl.find_opt map (x+1,y+1) in
    (* Some None Some is two distinct neighbors *)
    (* Some Some None is one distinct neighbor *)
    (* None Some Some is one distinct neighbor *)
    let b_bors = match bottom with
    | Some nbor -> (*Printf.printf "B: %d" nbor;*) nbor :: []
    | None -> (
        match (bottom_left, bottom_right) with
            | (Some l, Some r) -> (*Printf.printf "Two nbors: %d, %d\n" l r;*) (l :: r :: [])
            | (Some l, None) -> (*Printf.printf "BL: %d\n" l;*) l :: []
            | (None, Some r) -> (*Printf.printf "BR: %d\n" r;*) r :: []
            | (None, None) -> Printf.printf ""; []
    ) in
    (* left *)
    let l_bors = match (
        Hashtbl.find_opt map (x-1,y)
    ) with
    | Some e1 -> (
            (*Printf.printf "left neighbor: %d\n" e1;*) e1 :: []
        )
    | _ -> []
    in
    (* right *)
    let r_bors = match (
        Hashtbl.find_opt map (x+1,y)
    ) with
    | Some e1 -> (
            (*Printf.printf "right neighbor: %d\n" e1;*) e1 :: []
        )
    | _ -> []
    in
    append t_bors (append b_bors (append r_bors l_bors))


let solve_p1 (lines: string list): int =
    let lines = Array.of_list lines in
    let p_t_num = Hashtbl.create 1024 in
    for y = 0 to Array.length lines - 1 do
        let line = lines.(y) in
        (*print_endline line;*)
        let i_s = ref 0 in
        let in_num = ref false in
        let pts = Array.make 3 None in
        let pts_idx = ref 0 in
        for x = 1 to String.length line do
            let sub_str = (String.sub line !i_s (x - !i_s)) in
            (*Printf.printf "%d..%d: [%s]" !i_s x sub_str;*)
            if (is_num sub_str) then (
                (*Printf.printf "num-[%s](%d,%d) " sub_str (x-1) y;*)
                in_num := true;
                pts.(!pts_idx) <- Some ((x-1), y);
                pts_idx := !pts_idx + 1
            ) else ();
            if not (is_num sub_str) || x = String.length line then (
                let num_str = String.sub line !i_s ( if x = String.length line && is_num sub_str then x - !i_s else (x - !i_s - 1)) in
                (*Printf.printf "num_str: %s\n" num_str;*)
                (*let _ = if x = String.length line then Printf.printf "eol" else () in*)
                if (!in_num || x = String.length line) && String.length num_str > 0 && is_num num_str then (
                    let num = int_of_string num_str in
                    for i = 0 to !pts_idx - 1 do
                        match pts.(i) with
                        | Some pt -> (
                            (*Printf.printf "%d,%d\n" (fst pt) (snd pt);*)
                            Hashtbl.add p_t_num pt num
                        )
                        | None -> ();
                    done;
                    (*Printf.printf "[%s]" num_str;*)
                    pts_idx := 0
                )
                else ();
                i_s := x;
                in_num := false
            ) else ();
        done;
        (*Printf.printf "\n"*)
    done;
    let total = ref 0 in
    for y = 0 to Array.length lines - 1 do
        let line = lines.(y) in
        for x = 0 to String.length line - 1 do
            let curr = String.sub line x 1 in
            if not (is_num curr) && not (curr = ".") then
                let nbors = get_nbors p_t_num (x,y) in
                (*Printf.printf "(%d,%d): %s\n" x y curr;*)
                total := !total + (List.fold_left (fun a b -> a + b) 0 nbors)
        done;
        (*Printf.printf "\n"*)
    done;
    !total

let solve_p2 (lines: string list): int =
    let lines = Array.of_list lines in
    let p_t_num = Hashtbl.create 1024 in
    for y = 0 to Array.length lines - 1 do
        let line = lines.(y) in
        (*print_endline line;*)
        let i_s = ref 0 in
        let in_num = ref false in
        let pts = Array.make 3 None in
        let pts_idx = ref 0 in
        for x = 1 to String.length line do
            let sub_str = (String.sub line !i_s (x - !i_s)) in
            (*Printf.printf "%d..%d: [%s]" !i_s x sub_str;*)
            if (is_num sub_str) then (
                (*Printf.printf "num-[%s](%d,%d) " sub_str (x-1) y;*)
                in_num := true;
                pts.(!pts_idx) <- Some ((x-1), y);
                pts_idx := !pts_idx + 1
            ) else ();
            if not (is_num sub_str) || x = String.length line then (
                let num_str = String.sub line !i_s ( if x = String.length line && is_num sub_str then x - !i_s else (x - !i_s - 1)) in
                (*Printf.printf "num_str: %s\n" num_str;*)
                (*let _ = if x = String.length line then Printf.printf "eol" else () in*)
                if (!in_num || x = String.length line) && String.length num_str > 0 && is_num num_str then (
                    let num = int_of_string num_str in
                    for i = 0 to !pts_idx - 1 do
                        match pts.(i) with
                        | Some pt -> (
                            (*Printf.printf "%d,%d\n" (fst pt) (snd pt);*)
                            Hashtbl.add p_t_num pt num
                        )
                        | None -> ();
                    done;
                    (*Printf.printf "[%s]" num_str;*)
                    pts_idx := 0
                )
                else ();
                i_s := x;
                in_num := false
            ) else ();
        done;
        (*Printf.printf "\n"*)
    done;
    let total = ref 0 in
    for y = 0 to Array.length lines - 1 do
        let line = lines.(y) in
        for x = 0 to String.length line - 1 do
            let curr = String.sub line x 1 in
            if curr = "*" then
                let nbors = get_nbors p_t_num (x,y) in
                if List.length nbors = 2 then (
                    total := !total + ((List.hd nbors) * (List.hd (List.rev nbors)));
                ) else ();
                (*Printf.printf "(%d,%d): %s\n" x y curr;*)
        done;
        (*Printf.printf "\n"*)
    done;
    !total

let solve () =
    let test_lines = Aoc23.file_lines "d3s" in
    assert ((solve_p1 test_lines) = 4361);
    let lines = Aoc23.file_lines "d3i" in
    Printf.printf "Part 1: %d\n" (solve_p1 lines);
    let test_lines = Aoc23.file_lines "d3s" in
    assert ((solve_p2 test_lines) = 467835);
    let lines = Aoc23.file_lines "d3i" in
    Printf.printf "Part 2: %d\n" (solve_p2 lines)

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
