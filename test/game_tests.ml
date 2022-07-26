open Ocaml_2048.Game
open OUnit2

let () = test_func "testing..."


let (t1_board: board) = [
    [1; 2; 3; 4];
    [5; 6; 7; 8];
    [9; 10; 11; 12];
    [13; 14; 15; 16]
]


let test_rotation _ =
    (* board should stay that same after 4 90* rotations *)
    assert_equal (t1_board) (t1_board |> rot_4x4_board |> rot_4x4_board |> rot_4x4_board |> rot_4x4_board)
let test_flip _ = 
    assert_equal (t1_board) (t1_board |> flip_4x4_board |> flip_4x4_board)


let test_collapse_r1 _ =
    let row = [0; 2; 2; 0] in
    assert_equal (collapse_row row) [4; 0; 0; 0]
let test_collapse_r2 _ =
    let row = [2; 0; 2; 0] in
    assert_equal (collapse_row row) [4; 0; 0; 0]
let test_collapse_r3 _ =
    let row = [2; 2; 2; 2] in
    assert_equal (collapse_row row) [4; 4; 0; 0]
let test_collapse_r4 _ =
    let row = [0; 0; 0; 2] in
    assert_equal (collapse_row row) [2; 0; 0; 0]
let test_collapse_r5 _ =
    let row = [8; 8; 8; 4] in
    assert_equal (collapse_row row) [16; 8; 4; 0]

let test_suite =
  "Collapse Tests" >::: [
    "t1" >:: test_collapse_r1;
    "t2" >:: test_collapse_r2;
    "t3" >:: test_collapse_r3;
    "t4" >:: test_collapse_r4;
    "t5" >:: test_collapse_r5;
    "r1" >:: test_rotation;
    "r2" >:: test_flip;
  ]

let () =
  run_test_tt_main test_suite
