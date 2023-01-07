open! Core
open Shared

let%expect_test "T-Digests in which each point becomes a centroid" =
  (* consumes a point *)
  Tdigest.create () |> Tdigest.add ~data:0. |> check;
  [%expect {| (((mean 0) (n 1))) |}];

  (* consumes two points *)
  Tdigest.create () |> Tdigest.add_list [ 0.; 1. ] |> check;
  [%expect {| (((mean 0) (n 1)) ((mean 1) (n 1))) |}];

  (* consumes three points *)
  Tdigest.create () |> Tdigest.add_list [ 0.; 1.; -1. ] |> check;
  [%expect {| (((mean -1) (n 1)) ((mean 0) (n 1)) ((mean 1) (n 1))) |}];

  (* consumes increasing-valued points *)
  let () =
    let td =
      let xs = List.init 100 ~f:(fun i -> i * 10 |> Float.of_int) in
      Tdigest.create ~delta:(Tdigest.Merging 0.001) ~k:Tdigest.Manual () |> Tdigest.add_list xs
    in
    expected_centroids td;
    [%expect {| Identical |}]
  in

  (* consumes decreasing-valued points *)
  let () =
    let td =
      let xs = List.init 100 ~f:(fun i -> (99 - i) * 10 |> Float.of_int) in
      Tdigest.create ~delta:(Tdigest.Merging 0.001) ~k:Tdigest.Manual () |> Tdigest.add_list xs
    in
    expected_centroids td;
    [%expect {| Identical |}]
  in
  ()

let%expect_test "T-Digests in which points are merged into centroids" =
  (* consumes same-valued points into a single point *)
  Tdigest.create () |> Fn.apply_n_times ~n:100 (Tdigest.add ~data:1000.) |> check;
  [%expect {| (((mean 1000) (n 100))) |}];

  (* handles multiple duplicates *)
  Tdigest.create ~delta:(Tdigest.Merging 1.) ~k:Tdigest.Manual ~cx:Tdigest.Always ()
  |> Fn.apply_n_times ~n:10 (fun td ->
         td |> Tdigest.add ~data:0. |> Tdigest.add ~data:1. |> Tdigest.add ~data:0.5)
  |> check;
  [%expect {| (((mean 0) (n 10)) ((mean 0.5) (n 10)) ((mean 1) (n 10))) |}]

let%expect_test "compress" =
  (* compresses points and preserves bounds *)
  let () =
    let xs = List.init 100 ~f:(fun i -> i * 10 |> Float.of_int) in
    let td = Tdigest.create ~delta:(Tdigest.Merging 0.001) ~k:Tdigest.Manual () |> Tdigest.add_list xs in
    (* must be 100 *)
    check_size td;

    let td = Tdigest.compress ~delta:(Tdigest.Merging 0.1) td in
    (* must be < 100 *)
    check_size td;
    check_min_max td;
    [%expect {|
      100
      45
      (((0 1)) ((990 1))) |}]
  in

  (* K automatically compresses during ingest *)
  let () =
    let td =
      Array.init 10_000 ~f:(fun i -> i * 10 |> Float.of_int)
      |> Array.fold ~init:(Tdigest.create ()) ~f:(fun td x -> Tdigest.add td ~data:x)
    in
    (* must be < 10_000 *)
    check_size td;
    check_min_max td;
    [%expect {|
      2156
      (((0 1)) ((99990 1))) |}]
  in
  ()

let%expect_test "percentile ranks" =
  (* reports None when given no points *)
  Tdigest.create () |> check_p_rank 1.;
  [%expect {| () |}];

  (* from a single point *)
  Tdigest.create () |> Tdigest.add ~data:0. |> check_p_ranks [ -0.5; 0.; 0.5; 1.; 1.5 ];
  [%expect {| ((0) (0.5) (1) (1) (1)) |}];

  (* from three points *)
  Tdigest.create ()
  |> Tdigest.add_list [ -1.; 0.; 1. ]
  |> check_p_ranks [ -1.5; -1.0; -0.5; 0.; 0.5; 1.0; 1.5 ];
  [%expect
    {|
    ((0) (0.16666666666666666) (0.33333333333333331) (0.5) (0.66666666666666663)
     (0.83333333333333337) (1)) |}];

  (* from three points is same as from multiples of those points *)
  let () =
    let ps = [ -1.5; -1.0; -0.5; 0.; 0.5; 1.0; 1.5 ] in
    let td = Tdigest.create () |> Tdigest.add_list [ 0.; 1.; -1. ] in
    check_percentiles ps td;
    let td = td |> Tdigest.add_list [ 0.; 1.; -1. ] |> Tdigest.add_list [ 0.; 1.; -1. ] in
    check_percentiles ps td;
    [%expect {|
      ((-1) (-1) (-1) (-1) (0) (1) (1))
      ((-1) (-1) (-1) (-1) (0) (1) (1)) |}]
  in

  (* from four points away from the origin *)
  let () =
    Tdigest.create ()
    |> Tdigest.add_list [ 10.; 11.; 12.; 13. ]
    |> check_p_ranks [ 9.; 10.; 11.; 12.; 13.; 14. ];
    [%expect {| ((0) (0.125) (0.375) (0.625) (0.875) (1)) |}]
  in

  (* from four points is same as from multiples of those points *)
  let () =
    let ps = [ 9.; 10.; 11.; 12.; 13.; 14. ] in
    let td =
      Tdigest.create ~delta:(Tdigest.Merging 0.) ~k:Tdigest.Manual ()
      |> Tdigest.add_list [ 10.; 11.; 12.; 13. ]
    in
    check_p_ranks ps td;
    let td = td |> Tdigest.add_list [ 10.; 11.; 12.; 13. ] |> Tdigest.add_list [ 10.; 11.; 12.; 13. ] in
    check_p_ranks ps td;
    [%expect
      {|
      ((0) (0.125) (0.375) (0.625) (0.875) (1))
      ((0) (0.125) (0.375) (0.625) (0.875) (1)) |}]
  in

  (* from lots of uniformly distributed points *)
  let () =
    let td =
      Tdigest.create ()
      |> Fn.apply_n_times ~n:100_000 (fun td -> Tdigest.add td ~data:(Random.float 1.))
      |> Tdigest.compress
    in
    let _i, max_err, _td =
      Fn.apply_n_times ~n:100
        (fun (i, max_err, td) ->
          let td, q = Tdigest.p_rank td i |> Tuple2.map_snd ~f:(fun x -> Option.value_exn x) in
          let m = Float.(max max_err (i - q |> abs)) in
          Float.(i + 0.01), m, td)
        (0.01, 0.0, td)
    in
    Float.to_string max_err |> print_endline;
    (* must be < 0.01 *)
    [%expect {| 0.0020962038264262794 |}]
  in

  (* from an exact match *)
  let () =
    Tdigest.create ~delta:(Tdigest.Merging 0.001) ~k:Tdigest.Manual ()
    |> Fn.apply_n_times ~n:10 (Tdigest.add_list [ 10.; 20.; 30. ])
    |> check_p_rank 20.;
    [%expect {| (0.5) |}]
  in
  ()

let%expect_test "percentiles" =
  (* reports None when given no points *)
  Tdigest.create () |> check_percentile 0.5;
  [%expect {| () |}];

  (* from a single point *)
  Tdigest.create () |> Tdigest.add ~data:0. |> check_percentiles [ 0.; 0.5; 1. ];
  [%expect {| ((0) (0) (0)) |}];

  (* from two points *)
  Tdigest.create ()
  |> Tdigest.add_list [ 0.; 1. ]
  |> check_percentiles [ -1 // 4; 0.; 1 // 4; 1 // 2; 5 // 8; 3 // 4; 1.; 1.25 ];
  [%expect {| ((0) (0) (0) (0.5) (0.75) (1) (1) (1)) |}];

  (* from three points *)
  Tdigest.create ()
  |> Tdigest.add_list [ 0.; 0.5; 1. ]
  |> check_percentiles [ 0.; 1 // 4; 1 // 2; 3 // 4; 1. ];
  [%expect {| ((0) (0.125) (0.5) (0.875) (1)) |}];

  (* from four points *)
  Tdigest.create ()
  |> Tdigest.add_list [ 10.; 11.; 12.; 13. ]
  |> check_percentiles [ 0.; 1 // 4; 1 // 2; 3 // 4; 1. ];
  [%expect {| ((10) (10.5) (11.5) (12.5) (13)) |}];

  (* from lots of uniformly distributed points *)
  let () =
    let td =
      Tdigest.create ()
      |> Fn.apply_n_times ~n:100_000 (fun td -> Tdigest.add td ~data:(Random.float 1.))
      |> Tdigest.compress
    in
    let _i, max_err, _td =
      Fn.apply_n_times ~n:100
        (fun (i, max_err, td) ->
          let td, q = Tdigest.p_rank td i |> Tuple2.map_snd ~f:(fun x -> Option.value_exn x) in
          let m = Float.(max max_err (i - q |> abs)) in
          Float.(i + 0.01), m, td)
        (0.01, 0.0, td)
    in
    Float.to_string max_err |> print_endline;
    (* must be < 0.01 *)
    [%expect {| 0.0020962038264262794 |}]
  in
  ()

let%expect_test "serialization" =
  (* identical after recreating *)
  let xs = List.init 10 ~f:(fun _i -> Random.float 1.) in
  let td = Tdigest.create () |> Tdigest.add_list xs in
  let td1, export = Tdigest.to_string td in
  if String.length export <> 160 then failwith "export length <> 160";
  let td2 = Tdigest.of_string export in
  check td1;
  check td2;
  [%expect
    {|
    (((mean 0.11359872617230203) (n 1)) ((mean 0.14207889800896825) (n 1))
     ((mean 0.26565242651667725) (n 1)) ((mean 0.32088115017788221) (n 1))
     ((mean 0.41519876713081461) (n 1)) ((mean 0.43630556398799153) (n 1))
     ((mean 0.45062527388924589) (n 1)) ((mean 0.56883568253605243) (n 1))
     ((mean 0.64030838064885942) (n 1)) ((mean 0.96035328719918678) (n 1)))
    (((mean 0.11359872617230203) (n 1)) ((mean 0.14207889800896825) (n 1))
     ((mean 0.26565242651667725) (n 1)) ((mean 0.32088115017788221) (n 1))
     ((mean 0.41519876713081461) (n 1)) ((mean 0.43630556398799153) (n 1))
     ((mean 0.45062527388924589) (n 1)) ((mean 0.56883568253605243) (n 1))
     ((mean 0.64030838064885942) (n 1)) ((mean 0.96035328719918678) (n 1))) |}]

let%expect_test "merge" =
  (* incorporates all points *)
  let xs1 = [ 3.0; 4.0; 3.5; 7.0 ] in
  let xs2 = [ 3.0; 1.0; 6.5; 9.0 ] in
  let td1 = Tdigest.create () |> Tdigest.add_list (xs1 @ xs2) in
  let td2 =
    let a = Tdigest.create () |> Tdigest.add_list xs1 in
    let b = Tdigest.create () |> Tdigest.add_list xs2 in
    Tdigest.merge [ a; b ]
  in
  let ps = [ 0.0; 0.25; 0.50; 0.75; 1.0 ] in
  check_percentiles ps td1;
  check_percentiles ps td2;
  [%expect {|
    ((1) (3) (3.75) (6.75) (9))
    ((1) (3) (3.75) (6.75) (9)) |}]

let%expect_test "is_empty" =
  let xs = [ 3.0; 4.0; 3.5; 7.0 ] in
  let td = Tdigest.create () in
  Tdigest.is_empty td |> Bool.to_string |> print_endline;
  let td = Tdigest.add_list xs td in
  Tdigest.is_empty td |> Bool.to_string |> print_endline;
  [%expect {|
    true
    false |}]
