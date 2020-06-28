open! Core_kernel

open Shared

let () =
  Alcotest.run "Discrete" [
    "discrete digests", [

      "consumes increasing-valued points", `Quick, (fun () ->
        let xs = List.init 100 ~f:(fun i -> i * 10 |> Float.of_int) in
        let td =
          Tdigest.create ~delta:(Tdigest.Discrete) ()
          |> Tdigest.add_list xs
        in
        let against = List.init 100 ~f:(fun i -> b (i * 10 |> Float.of_int) 1) in
        check td against
      );

      "consumes decreasing-valued points", `Quick, (fun () ->
        let xs = List.init 100 ~f:(fun i -> (99 - i) * 10 |> Float.of_int) in
        let td =
          Tdigest.create ~delta:(Tdigest.Discrete) ()
          |> Tdigest.add_list xs
        in
        let against = List.init 100 ~f:(fun i -> b (i * 10 |> Float.of_int) 1) in
        check td against
      );

      "consumes same-valued points into a single point", `Quick, (fun () ->
        let td =
          Tdigest.create ~delta:(Tdigest.Discrete) ()
          |> Fn.apply_n_times ~n:100 (Tdigest.add ~data:1000.)
        in
        check td [b 1000. 100]
      );

      "selects a run of duplicates containing the percentile", `Quick, (fun () ->
        let td =
          Tdigest.create ~delta:(Tdigest.Discrete) ()
          |> Tdigest.add_list [5.; 0.; 0.; 8.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 3.; 0.; 0.; 0.; 0.; 6.; 1.; 0.; 6.; 5.; 3.; 6.; 1.; 1.; 0.; 0.; 1.; 1.; 0.; 0.; 1.; 0.]
        in
        let _td, q = Tdigest.percentile td 0.5 in
        if Float.((Option.value_exn q) <> 0.) then failwith "percentile(0.5) <> 0.0"
      );

      "handles multiples duplicates", `Quick, (fun () ->
        let td =
          Tdigest.create ~delta:(Tdigest.Discrete) ()
          |> Fn.apply_n_times ~n:10 (fun td ->
            td
            |> Tdigest.add ~data:0.
            |> Tdigest.add ~data:1.
            |> Tdigest.add ~data:0.5
          )
        in
        check td [b 0. 10; b 0.5 10; b 1. 10]
      );

    ];
    "discrete percentile ranks", [

      "from a single point", `Quick, (fun () ->
        let td =
          Tdigest.create ~delta:(Tdigest.Discrete) ()
          |> Tdigest.add ~data:0.
        in
        check_p_ranks td
          [-1.5; 0.; 0.5; 1.; 1.5]
          [0.; 1.; 1.; 1.; 1.]
      );

      "from two points", `Quick, (fun () ->
        let td =
          Tdigest.create ~delta:(Tdigest.Discrete) ()
          |> Tdigest.add_list [0.; 1.]
        in
        check_p_ranks td
          [-1.5; 0.; 0.5; 1.; 1.5]
          [0.; 0.5; 0.5; 1.; 1.]
      );

      "from three points", `Quick, (fun () ->
        let td =
          Tdigest.create ~delta:(Tdigest.Discrete) ()
          |> Tdigest.add_list [-1.; 0.; 1.]
        in
        check_p_ranks td
          [-1.5; -1.; -0.5; 0.; 0.5; 1.; 1.5]
          [0.; 1//3; 1//3; 2//3; 2//3; 1.; 1.]
      );

      "from three points is same as from multiples of those points", `Quick, (fun () ->
        let td =
          Tdigest.create ~delta:(Tdigest.Discrete) ()
          |> Tdigest.add_list [0.; 1.; -1.]
        in
        let td, result1 = Tdigest.p_ranks td [-1.5; -1.; -0.5; 0.; 0.5; 1.; 1.5] in
        let td =
          td
          |> Tdigest.add_list [0.; 1.; -1.]
          |> Tdigest.add_list [0.; 1.; -1.]
        in
        let _td, result2 = Tdigest.p_ranks td [-1.5; -1.; -0.5; 0.; 0.5; 1.; 1.5] in
        Json_diff.assert_equal (float_opts_to_yojson result1) (float_opts_to_yojson result2)
      );

      "from four points away from the origin", `Quick, (fun () ->
        let td =
          Tdigest.create ~delta:(Tdigest.Discrete) ()
          |> Tdigest.add_list [10.; 11.; 12.; 13.]
        in
        check_p_ranks td
          [9.; 10.; 11.; 12.; 13.; 14.]
          [0.; 1//4; 2//4; 3//4; 1.; 1.]
      );

      "from four points is same as from multiples of those points", `Quick, (fun () ->
        let td =
          Tdigest.create ~delta:(Tdigest.Discrete) ()
          |> Tdigest.add_list [10.; 11.; 12.; 13.]
        in
        let td, result1 = Tdigest.p_ranks td [9.; 10.; 11.; 12.; 13.; 14.] in
        let td =
          td
          |> Tdigest.add_list [10.; 11.; 12.; 13.]
          |> Tdigest.add_list [10.; 11.; 12.; 13.]
        in
        let _td, result2 = Tdigest.p_ranks td [9.; 10.; 11.; 12.; 13.; 14.] in
        Json_diff.assert_equal (float_opts_to_yojson result1) (float_opts_to_yojson result2)
      );

    ];

    "discrete percentiles", [

      "from a single point", `Quick, (fun () ->
        let td =
          Tdigest.create ~delta:(Tdigest.Discrete) ()
          |> Tdigest.add ~data:0.
        in
        check_percentiles td
          [0.; 0.5; 1.]
          [0.; 0.; 0.]
      );

      "from two points", `Quick, (fun () ->
        let td =
          Tdigest.create ~delta:(Tdigest.Discrete) ()
          |> Tdigest.add_list [0.; 10.]
        in
        check_percentiles td
          [0.; 1//4; 1//2; 3//4; 1.]
          [0.; 0.; 0.; 10.; 10.]
      );

      "from three points", `Quick, (fun () ->
        let td =
          Tdigest.create ~delta:(Tdigest.Discrete) ()
          |> Tdigest.add_list [0.; 5.; 10.]
        in
        check_percentiles td
          [0.; 1//4; Float.(1./2.9); 1//2; 2//3; 3//4; 1.]
          [0.; 0.; 5.; 5.; 5.; 10.; 10.]
      );

      "from four points away from the origin", `Quick, (fun () ->
        let td =
          Tdigest.create ~delta:(Tdigest.Discrete) ()
          |> Tdigest.add_list [10.; 11.; 12.; 13.]
        in
        check_percentiles td
          [0.; 1//4; 1//2; 3//4; 1.]
          [10.; 10.; 11.; 12.; 13.]
      );

    ]
  ]
