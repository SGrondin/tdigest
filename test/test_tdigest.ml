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
       td |> Tdigest.add ~data:0. |> Tdigest.add ~data:1. |> Tdigest.add ~data:0.5 )
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
      44
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
      2132
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
    [%expect {| 0.0038269003338100016 |}]
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
    [%expect {| 0.0038269003338100016 |}]
  in
  ()

let%expect_test "serialization" =
  (* identical after recreating (to_string/of_string) *)
  let () =
    let xs = List.init 10 ~f:(fun _i -> Random.float 1.) in
    let td = Tdigest.create () |> Tdigest.add_list xs in
    let td1, export = Tdigest.to_string td in
    if String.length export <> 160 then failwith "export length <> 160";
    let td2 = Tdigest.of_string export in
    check td1;
    check td2;
    [%expect
      {|
    (((mean 0.0278013320652886) (n 1)) ((mean 0.088701617893937754) (n 1))
     ((mean 0.25409775932960554) (n 1)) ((mean 0.29053618718083241) (n 1))
     ((mean 0.48715518185429985) (n 1)) ((mean 0.55401752229333867) (n 1))
     ((mean 0.63535141231675607) (n 1)) ((mean 0.68236915472644311) (n 1))
     ((mean 0.77208758810861389) (n 1)) ((mean 0.78255046724687982) (n 1)))
    (((mean 0.0278013320652886) (n 1)) ((mean 0.088701617893937754) (n 1))
     ((mean 0.25409775932960554) (n 1)) ((mean 0.29053618718083241) (n 1))
     ((mean 0.48715518185429985) (n 1)) ((mean 0.55401752229333867) (n 1))
     ((mean 0.63535141231675607) (n 1)) ((mean 0.68236915472644311) (n 1))
     ((mean 0.77208758810861389) (n 1)) ((mean 0.78255046724687982) (n 1))) |}]
  in

  (* identical after recreating (sexp) *)
  let () =
    let xs = List.init 10 ~f:(fun _i -> Random.float 1.) in
    let td1 = Tdigest.create () |> Tdigest.add_list xs in
    let td2 = [%sexp_of: Tdigest.t] td1 |> [%of_sexp: Tdigest.t] in
    print_endline (sprintf !"%{sexp#hum: Tdigest.t}" td1);
    print_endline (sprintf !"%{sexp#hum: Tdigest.t}" td2);
    check td1;
    check td2;
    [%expect
      {|
      ((settings ((delta (Merging 0.01)) (k (Automatic 25)) (cx (Growth 1.1))))
       (state
        "E\160\000<T\236\179?\000\000\000\000\000\000\240?\003\225*G\145\159\181?\000\000\000\000\000\000\240?\019Jvk\146?\195?\000\000\000\000\000\000\240?_\136\253\005\214%\206?\000\000\000\000\000\000\240?\186\158o\233\235\187\209?\000\000\000\000\000\000\240?\007\024\142!@-\212?\000\000\000\000\000\000\240?;\236\175\255<w\214?\000\000\000\000\000\000\240?\240v\249^8\220\220?\000\000\000\000\000\000\240?\127\152\241\150\165\206\227?\000\000\000\000\000\000\240?L !\229\227\166\231?\000\000\000\000\000\000\240?")
       (stats ((cumulates_count 10) (compress_count 0) (auto_compress_count 0))))
      ((settings ((delta (Merging 0.01)) (k (Automatic 25)) (cx (Growth 1.1))))
       (state
        "E\160\000<T\236\179?\000\000\000\000\000\000\240?\003\225*G\145\159\181?\000\000\000\000\000\000\240?\019Jvk\146?\195?\000\000\000\000\000\000\240?_\136\253\005\214%\206?\000\000\000\000\000\000\240?\186\158o\233\235\187\209?\000\000\000\000\000\000\240?\007\024\142!@-\212?\000\000\000\000\000\000\240?;\236\175\255<w\214?\000\000\000\000\000\000\240?\240v\249^8\220\220?\000\000\000\000\000\000\240?\127\152\241\150\165\206\227?\000\000\000\000\000\000\240?L !\229\227\166\231?\000\000\000\000\000\000\240?")
       (stats ((cumulates_count 10) (compress_count 0) (auto_compress_count 0))))
      (((mean 0.077824844979319144) (n 1)) ((mean 0.084466056704403811) (n 1))
       ((mean 0.15037756201907423) (n 1)) ((mean 0.2355296639680686) (n 1))
       ((mean 0.27709481998907004) (n 1)) ((mean 0.3152618720708919) (n 1))
       ((mean 0.35102772683575595) (n 1)) ((mean 0.45094117426729863) (n 1))
       ((mean 0.61897544366071588) (n 1)) ((mean 0.73912234069667582) (n 1)))
      (((mean 0.077824844979319144) (n 1)) ((mean 0.084466056704403811) (n 1))
       ((mean 0.15037756201907423) (n 1)) ((mean 0.2355296639680686) (n 1))
       ((mean 0.27709481998907004) (n 1)) ((mean 0.3152618720708919) (n 1))
       ((mean 0.35102772683575595) (n 1)) ((mean 0.45094117426729863) (n 1))
       ((mean 0.61897544366071588) (n 1)) ((mean 0.73912234069667582) (n 1))) |}]
  in

  (* identical after recreating (marshalling) *)
  let () =
    let xs = List.init 10 ~f:(fun _i -> Random.float 1.) in
    let td1 = Tdigest.Marshallable.create () |> Tdigest.Marshallable.add_list xs in
    let td2 : Tdigest.Marshallable.t =
      Marshal.to_string td1 [] |> fun s ->
      print_endline (sprintf "Length: %d" (String.length s));
      Marshal.from_string s 0
    in
    print_endline (sprintf !"%{sexp#hum: Tdigest.Marshallable.t}" td1);
    print_endline (sprintf !"%{sexp#hum: Tdigest.Marshallable.t}" td2);
    checkm td1;
    checkm td2;
    [%expect
      {|
      Length: 531
      ((settings ((delta (Merging 0.01)) (k (Automatic 25)) (cx (Growth 1.1))))
       (state
        "q\13752\204H\161?\000\000\000\000\000\000\240?)'\237\200\164\019\197?\000\000\000\000\000\000\240?\177\177\131SS\168\208?\000\000\000\000\000\000\240?\167\144'\003}/\224?\000\000\000\000\000\000\240?\182\188\187h\203o\225?\000\000\000\000\000\000\240?\133\132\245Gqp\228?\000\000\000\000\000\000\240?\130\246\223Pj\145\230?\000\000\000\000\000\000\240?\178<b\016yA\233?\000\000\000\000\000\000\240?`\197WT\rD\234?\000\000\000\000\000\000\240?b\020\186\238j\204\234?\000\000\000\000\000\000\240?")
       (stats ((cumulates_count 10) (compress_count 0) (auto_compress_count 0))))
      ((settings ((delta (Merging 0.01)) (k (Automatic 25)) (cx (Growth 1.1))))
       (state
        "q\13752\204H\161?\000\000\000\000\000\000\240?)'\237\200\164\019\197?\000\000\000\000\000\000\240?\177\177\131SS\168\208?\000\000\000\000\000\000\240?\167\144'\003}/\224?\000\000\000\000\000\000\240?\182\188\187h\203o\225?\000\000\000\000\000\000\240?\133\132\245Gqp\228?\000\000\000\000\000\000\240?\130\246\223Pj\145\230?\000\000\000\000\000\000\240?\178<b\016yA\233?\000\000\000\000\000\000\240?`\197WT\rD\234?\000\000\000\000\000\000\240?b\020\186\238j\204\234?\000\000\000\000\000\000\240?")
       (stats ((cumulates_count 10) (compress_count 0) (auto_compress_count 0))))
      (((mean 0.033758526925128936) (n 1)) ((mean 0.16466197787149753) (n 1))
       ((mean 0.26027377277093949) (n 1)) ((mean 0.50579691520808445) (n 1))
       ((mean 0.54489679771167832) (n 1)) ((mean 0.6387258916330586) (n 1))
       ((mean 0.70525089069242619) (n 1)) ((mean 0.78924229812573388) (n 1))
       ((mean 0.82080713723386722) (n 1)) ((mean 0.83745333315222248) (n 1)))
      (((mean 0.033758526925128936) (n 1)) ((mean 0.16466197787149753) (n 1))
       ((mean 0.26027377277093949) (n 1)) ((mean 0.50579691520808445) (n 1))
       ((mean 0.54489679771167832) (n 1)) ((mean 0.6387258916330586) (n 1))
       ((mean 0.70525089069242619) (n 1)) ((mean 0.78924229812573388) (n 1))
       ((mean 0.82080713723386722) (n 1)) ((mean 0.83745333315222248) (n 1))) |}]
  in
  ()

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
