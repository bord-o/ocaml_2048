open Ocaml_2048.Game
open Graphics

let (empty_cells: board) = [
    [0; 0; 0; 0];
    [0; 0; 0; 0];
    [0; 0; 0; 0];
    [0; 0; 0; 0]
]
let () = print_endline "Ocaml 2048!"

let draw_board b = 
    for i = 0 to 3 do 
        for j = 0 to 3 do
            (* print_endline @@ string_of_int @@ (i * (600/4)); *)
            set_color (match List.nth (List.nth (b |> flip_4x4_board) j) i with
            | 0 -> rgb 204 192 179
            | 2 -> rgb 238 228 218 
            | 4 -> rgb 237 224 200
            | 8 -> rgb 242 177 121
            | 16 -> rgb 245 149 99
            | 32 -> rgb 246 124 95
            | 64 -> rgb 246 94 59
            | 128 -> rgb 237 207 114
            | 256 -> rgb 237 204 97
            | 512 -> rgb 237 200 80
            | 1024 -> rgb 237 197 63
            | 2048 -> rgb 237 194 46
            | _ -> red
            );

            fill_rect (20+ (i * (600/4))) (20+ (j * (600/4))) 100 100 ;
            moveto (60 + (i * (600/4))) (60+ (j * (600/4)));
            set_color @@ rgb 128 128 128;
            set_font "-adobe-courier-bold-r-normal--34-240-100-100-m-200-iso8859-1";
            let v = List.nth (List.nth (b |> flip_4x4_board) j) i in
            draw_string (if v=0 then "" else string_of_int v);

        done
    done ;;

let game_iteration key (b: board) =
    let nb = 
        if key="a" then collapse_left b else
        if key="s" then collapse_down b else
        if key="d" then collapse_right b else
        if key="w" then collapse_up b else
            empty_cells
    in 
    let nnb = nb |> cell_init in
    draw_board nnb;
    nnb

let rec game_loop b = 
    let k = read_line () in
    let nb = game_iteration k b in

    match contains_2048 nb with
    | true -> empty_cells
    | false -> game_loop nb


let () = open_graph " 600x600+500+100";;
set_color @@ rgb 187 173 160;;
fill_rect 0 0 600 600;;

(* let _ = draw_board test_cells;; *)

let t = game_loop empty_cells;;
print_board t;;
