open! Core
open Shared

let%expect_test "discrete digests" =
  (* consumes increasing-valued points *)
  let () =
    let xs = List.init 100 ~f:(fun i -> i * 10 |> Float.of_int) in
    let td = Tdigest.create ~delta:Tdigest.Discrete () |> Tdigest.add_list xs in
    expected_centroids td;
    [%expect {| Identical |}]
  in

  (* consumes decreasing-valued points *)
  let () =
    let xs = List.init 100 ~f:(fun i -> (99 - i) * 10 |> Float.of_int) in
    let td = Tdigest.create ~delta:Tdigest.Discrete () |> Tdigest.add_list xs in
    expected_centroids td;
    [%expect {| Identical |}]
  in

  (* consumes same-valued points into a single point *)
  Tdigest.create ~delta:Tdigest.Discrete () |> Fn.apply_n_times ~n:100 (Tdigest.add ~data:1000.) |> check;
  [%expect {| (((mean 1000) (n 100))) |}];

  (* selects a run of duplicates containing the percentile *)
  Tdigest.create ~delta:Tdigest.Discrete ()
  |> Tdigest.add_list
       [
         5.;
         0.;
         0.;
         8.;
         0.;
         0.;
         0.;
         0.;
         0.;
         0.;
         0.;
         0.;
         0.;
         0.;
         0.;
         0.;
         0.;
         0.;
         3.;
         0.;
         0.;
         0.;
         0.;
         6.;
         1.;
         0.;
         6.;
         5.;
         3.;
         6.;
         1.;
         1.;
         0.;
         0.;
         1.;
         1.;
         0.;
         0.;
         1.;
         0.;
       ]
  |> check_percentile 0.5;
  [%expect {| (0) |}];

  (* handles multiples duplicates *)
  Tdigest.create ~delta:Tdigest.Discrete ()
  |> Fn.apply_n_times ~n:10 (fun td ->
       td |> Tdigest.add ~data:0. |> Tdigest.add ~data:1. |> Tdigest.add ~data:0.5 )
  |> check;
  [%expect {| (((mean 0) (n 10)) ((mean 0.5) (n 10)) ((mean 1) (n 10))) |}]

let%expect_test "discrete percentile ranks" =
  (* from a single point *)
  Tdigest.create ~delta:Tdigest.Discrete ()
  |> Tdigest.add ~data:0.
  |> check_p_ranks [ -1.5; 0.; 0.5; 1.; 1.5 ];
  [%expect {| ((0) (1) (1) (1) (1)) |}];

  (* from two points *)
  Tdigest.create ~delta:Tdigest.Discrete ()
  |> Tdigest.add_list [ 0.; 1. ]
  |> check_p_ranks [ -1.5; 0.; 0.5; 1.; 1.5 ];
  [%expect {| ((0) (0.5) (0.5) (1) (1)) |}];

  (* from three points *)
  Tdigest.create ~delta:Tdigest.Discrete ()
  |> Tdigest.add_list [ -1.; 0.; 1. ]
  |> check_p_ranks [ -1.5; -1.; -0.5; 0.; 0.5; 1.; 1.5 ];
  [%expect
    {|
    ((0) (0.33333333333333331) (0.33333333333333331) (0.66666666666666663)
     (0.66666666666666663) (1) (1)) |}];

  (* from three points is same as from multiples of those points *)
  let () =
    let ps = [ -1.5; -1.; -0.5; 0.; 0.5; 1.; 1.5 ] in
    let td1 = Tdigest.create ~delta:Tdigest.Discrete () |> Tdigest.add_list [ 0.; 1.; -1. ] in
    check_p_ranks ps td1;
    let td2 = td1 |> Tdigest.add_list [ 0.; 1.; -1. ] |> Tdigest.add_list [ 0.; 1.; -1. ] in
    check_p_ranks ps td2;

    [%expect
      {|
      ((0) (0.33333333333333331) (0.33333333333333331) (0.66666666666666663)
       (0.66666666666666663) (1) (1))
      ((0) (0.33333333333333331) (0.33333333333333331) (0.66666666666666663)
       (0.66666666666666663) (1) (1)) |}]
  in

  (* from four points away from the origin *)
  Tdigest.create ~delta:Tdigest.Discrete ()
  |> Tdigest.add_list [ 10.; 11.; 12.; 13. ]
  |> check_p_ranks [ 9.; 10.; 11.; 12.; 13.; 14. ];
  [%expect {| ((0) (0.25) (0.5) (0.75) (1) (1)) |}];

  (* from four points is same as from multiples of those points *)
  let () =
    let ps = [ 9.; 10.; 11.; 12.; 13.; 14. ] in
    let td1 = Tdigest.create ~delta:Tdigest.Discrete () |> Tdigest.add_list [ 10.; 11.; 12.; 13. ] in
    check_p_ranks ps td1;
    let td2 = td1 |> Tdigest.add_list [ 10.; 11.; 12.; 13. ] |> Tdigest.add_list [ 10.; 11.; 12.; 13. ] in
    check_p_ranks ps td2;
    [%expect {|
      ((0) (0.25) (0.5) (0.75) (1) (1))
      ((0) (0.25) (0.5) (0.75) (1) (1)) |}]
  in
  ()

let%expect_test "discrete percentiles" =
  (* from a single point *)
  Tdigest.create ~delta:Tdigest.Discrete () |> Tdigest.add ~data:0. |> check_percentiles [ 0.; 0.5; 1. ];
  [%expect {| ((0) (0) (0)) |}];

  (* from two points *)
  Tdigest.create ~delta:Tdigest.Discrete ()
  |> Tdigest.add_list [ 0.; 10. ]
  |> check_percentiles [ 0.; 1 // 4; 1 // 2; 3 // 4; 1. ];
  [%expect {| ((0) (0) (0) (10) (10)) |}];

  (* from three points *)
  Tdigest.create ~delta:Tdigest.Discrete ()
  |> Tdigest.add_list [ 0.; 5.; 10. ]
  |> check_percentiles [ 0.; 1 // 4; Float.(1. / 2.9); 1 // 2; 2 // 3; 3 // 4; 1. ];
  [%expect {| ((0) (0) (5) (5) (5) (10) (10)) |}];

  (* from four points away from the origin *)
  Tdigest.create ~delta:Tdigest.Discrete ()
  |> Tdigest.add_list [ 10.; 11.; 12.; 13. ]
  |> check_percentiles [ 0.; 1 // 4; 1 // 2; 3 // 4; 1. ];
  [%expect {| ((10) (10) (11) (12) (13)) |}]
