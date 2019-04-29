open OUnit2

open Util

let suite = "Util.List" >::: [
  "fold_unit_some" >::: [
    "some3" >:: (fun _ ->
      assert_equal (List.fold_until_some (function 3 -> Some 3 | _ -> None) [1;2;3;4]) (Some 3)
    ) ;
    "some1" >:: (fun _ ->
      assert_equal (List.fold_until_some (function 1 -> Some 1 | _ -> None) [1;2;3;4]) (Some 1)
    ) ;
    "none" >:: (fun _ ->
      assert_equal (List.fold_until_some (fun _ -> None) [1;2;3;4]) None
    )
  ]
];
