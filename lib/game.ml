type cell = int

type board = cell list list

let (test_cells: board) = [
    [1; 2; 3; 4];
    [5; 6; 7; 8];
    [9; 10; 11; 12];
    [13; 14; 15; 16]
]

let (empty_cells: board) = [
    [0; 0; 0; 0];
    [0; 0; 0; 0];
    [0; 0; 0; 0];
    [0; 0; 0; 0]
]

let (init_cells: board) = [
    [2; 2; 2; 2];
    [8; 8; 8; 4];
    [0; 0; 0; 2];
    [0; 2; 2; 2048]
]

let flip_4x4_board = function
    | [] -> []
    | [a; b; c; d] -> [d; c; b; a]
    | _ -> []

(* 90 deg to the right *)
let rot_4x4_board = function
    | [
        [a; b; c; d];
        [e; f; g; h];
        [i; j; k; l];
        [m; n; o; p]
    ]->[ 
        [m; i; e; a];
        [n; j; f; b];
        [o; k; g; c];
        [p; l; h; d]]
    | _ -> [[]]

let test_row = [8; 8; 8; 4]
(* collapses row to the left (head) *)
let rec slide_row = function
    | [x; y] -> if x=0 then [y; 0] else [x; y]
    | x::x'::xs -> if x=0 then slide_row (x'::xs) @ [0] else x:: (slide_row @@ x'::xs)
    | _ -> []

let rec take n l =
    let len = List.length l in
    if n >= len then
        l
    else if n=1 then
        [List.hd l]
    else if n=0 then
        []
    else
        match l with
        | [] -> []
        | x::xs -> x:: take (n-1) xs

let unflatten_board fb =
    [
        take 4 fb;
        take 4 (fb |> List.filteri (fun i _ -> i>=4));
        take 4 (fb |> List.filteri (fun i _ -> i>=8));
        take 4 (fb |> List.filteri (fun i _ -> i>=12));
    ]

let  merge_two = function
    | [] -> []
    | [x; y] -> (
        if x=y then [(x*2); 0] else
        if x=0 then [y; 0] else
        [x; y]
    )
    | _ -> []

let collapse_row r = 
    let nr = match slide_row r with
    | [a; b; c; d] -> merge_two [a; b] @ merge_two [c; d]
    | _ -> [] 
    in
    match nr with 
    | [a; b; c; d] -> let merged = a :: (merge_two [b; c]) @ [d] in
        slide_row merged
    | _ -> []
    
let collapse_left (b: board) =
    List.map collapse_row b

let collapse_right (b: board) =
    b |> rot_4x4_board |> rot_4x4_board |> collapse_left |> rot_4x4_board |>rot_4x4_board 

let collapse_up (b: board) =
    b |> rot_4x4_board |>rot_4x4_board |> rot_4x4_board |> collapse_left |> rot_4x4_board 

let collapse_down (b: board) =
    b |> rot_4x4_board |> collapse_left |> rot_4x4_board |> rot_4x4_board |> rot_4x4_board

let index_empty_cells (b: board) =
    List.filter (fun x -> x<>(-1)) @@ List.mapi (fun x y -> if y=0 then x else -1) (List.flatten b)

let init_two i flat_board =
    flat_board |> List.mapi (fun index x -> if index=i then 2 else x)

let contains_2048 (b: board) =
    let y = b |> List.flatten |> List.filter (fun x -> x=2048) |> List.length in
    if y<>0 then true else false

let cell_init (b: board) =
    Random.self_init ();
    let empty = List.length @@ index_empty_cells b in
    let rnum = Random.int empty in
    let new_two = List.nth (index_empty_cells b) rnum in
    b |> List.flatten |> init_two new_two |> unflatten_board
    

let string_of_int_list l = String.concat " "  ( List.map string_of_int l )
let string_of_board b = String.concat "\n" (List.map string_of_int_list b)
let print_board b = print_endline @@ string_of_board b

