open! Core_kernel

open Shared

let () =
  Alcotest.run "TDigest" [
    "T-Digests in which each point becomes a centroid", [

      "consumes a point", `Quick, (fun () ->
        let td =
          Tdigest.create ()
          |> Tdigest.add ~mean:0.
        in
        check td [b 0. 1]
      );

      "consumes two points", `Quick, (fun () ->
        let td =
          Tdigest.create ()
          |> Tdigest.add_list [0.; 1.]
        in
        check td [b 0. 1; b 1. 1]
      );

      "consumes three points", `Quick, (fun () ->
        let td =
          Tdigest.create ()
          |> Tdigest.add_list [0.; 1.; -1.]
        in
        check td [b (-1.) 1; b 0. 1; b 1. 1]
      );

      "consumes increasing-valued points", `Quick, (fun () ->
        let xs = List.init 100 ~f:(fun i -> i * 10 |> Float.of_int) in
        let td =
          Tdigest.create ~delta:(Tdigest.Compress 0.001) ~k:0 ()
          |> Tdigest.add_list xs
        in
        let against = List.init 100 ~f:(fun i -> b (i * 10 |> Float.of_int) 1) in
        check td against
      );

      "consumes decreasing-valued points", `Quick, (fun () ->
        let xs = List.init 100 ~f:(fun i -> (99 - i) * 10 |> Float.of_int) in
        let td =
          Tdigest.create ~delta:(Tdigest.Compress 0.001) ~k:0 ()
          |> Tdigest.add_list xs
        in
        let against = List.init 100 ~f:(fun i -> b (i * 10 |> Float.of_int) 1) in
        check td against
      );

    ];
    "T-Digests in which points are merged into centroids", [

      "consumes same-valued points into a single point", `Quick, (fun () ->
        let td =
          Tdigest.create ()
          |> Fn.apply_n_times
            ~n:100
            (Tdigest.add ~mean:1000.)
        in
        check td [b 1000. 100]
      );

      "handles multiple duplicates", `Quick, (fun () ->
        let td =
          Tdigest.create ~delta:(Tdigest.Compress 1.) ~k:0 ~cx:0. ()
          |> Fn.apply_n_times
            ~n:10
            (fun td ->
                td
                |> Tdigest.add ~mean:0.
                |> Tdigest.add ~mean:1.
                |> Tdigest.add ~mean:0.5
            )
        in
        check td [b 0. 10; b 0.5 10; b 1. 10]
      );

    ];
    "compress", [

      "compresses points and preserves bounds", `Quick, (fun () ->
        let xs = List.init 100 ~f:(fun i -> i * 10 |> Float.of_int) in
        let td =
          Tdigest.create ~delta:(Tdigest.Compress 0.001) ~k:0 ()
          |> Tdigest.add_list xs
        in

        let size = Tdigest.size td in
        if size <> 100 then failwithf "Original size: %d <> 100" size ();

        let td = Tdigest.Testing.compress_with_delta td (Tdigest.Compress 0.1) in
        let size = Tdigest.size td in
        if size >= 100 then failwithf "Compressed size: %d >= 100" size ();

        Json_diff.assert_equal (Tdigest.Testing.min td) (b 0. 1 |> basic_to_yojson);
        Json_diff.assert_equal (Tdigest.Testing.max td) (b 990. 1 |> basic_to_yojson)
      );

      "K automatically compresses during ingest", `Quick, (fun () ->
        let td =
          Array.init 10_000 ~f:(fun i -> i * 10 |> Float.of_int)
          |> Array.fold ~init:(Tdigest.create ()) ~f:(fun td x -> Tdigest.add td ~mean:x)
        in

        let size = Tdigest.size td in
        if size >= 10_000 then failwithf "Size: %d >= 10,000" size ();

        Json_diff.assert_equal (Tdigest.Testing.min td) (b 0. 1 |> basic_to_yojson);
        Json_diff.assert_equal (Tdigest.Testing.max td) (b 99_990. 1 |> basic_to_yojson)
      );

    ];

    "percentile ranks", [

      "reports None when given no points", `Quick, (fun () ->
        let td = Tdigest.create () in
        if Option.is_some (Tdigest.p_rank td 1. |> snd) then failwith "p_rank of empty should be None"
      );

      "from a single point", `Quick, (fun () ->
        let td =
          Tdigest.create ()
          |> Tdigest.add ~mean:0.
        in
        check_p_ranks td
          [-0.5; 0.; 0.5; 1.; 1.5]
          [0.; 0.5; 1.; 1.; 1.]
      );

      "from two points", `Quick, (fun () ->
        let td =
          Tdigest.create ()
          |> Tdigest.add_list [0.; 1.]
        in
        check_p_ranks td
          [-0.5; 0.; 0.5; 1.; 1.5]
          [0.; 0.25; 0.5; 0.75; 1.]
      );

      "from three points", `Quick, (fun () ->
        let td =
          Tdigest.create ()
          |> Tdigest.add_list [-1.; 0.; 1.]
        in
        check_p_ranks td
          [-1.5; -1.0; -0.5; 0.; 0.5; 1.0; 1.5]
          [0.; 1//6; 2//6; 3//6; 4//6; 5//6; 1.]
      );

      "from three points is same as from multiples of those points", `Quick, (fun () ->
        let td =
          Tdigest.create ()
          |> Tdigest.add_list [0.; 1.; -1.]
        in
        let td, result1 = Tdigest.percentiles td [-1.5; -1.0; -0.5; 0.; 0.5; 1.0; 1.5]in
        let td =
          td
          |> Tdigest.add_list [0.; 1.; -1.]
          |> Tdigest.add_list [0.; 1.; -1.]
        in
        let _td, result2 = Tdigest.percentiles td [-1.5; -1.0; -0.5; 0.; 0.5; 1.0; 1.5] in
        Json_diff.assert_equal (float_opts_to_yojson result1) (float_opts_to_yojson result2)
      );

      "from four points away from the origin", `Quick, (fun () ->
        let td =
          Tdigest.create ()
          |> Tdigest.add_list [10.; 11.; 12.; 13.]
        in
        check_p_ranks td
          [9.; 10.; 11.; 12.; 13.; 14.]
          [0.; 1//8; 3//8; 5//8; 7//8; 1.]
      );

      "from four points is same as from multiples of those points", `Quick, (fun () ->
        let td =
          Tdigest.create ~delta:(Tdigest.Compress 0.) ~k:0 ()
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

      "from lots of uniformly distributed points", `Quick, (fun () ->
        let td =
          Tdigest.create ()
          |> Fn.apply_n_times
            ~n:100_000
            (fun td -> Tdigest.add td ~mean:(Random.float 1.))
          |> Tdigest.Testing.compress
        in
        let _i, max_err, _td =
          Fn.apply_n_times ~n:100 (fun (i, max_err, td) ->
            let td, q = Tdigest.p_rank td i |> Tuple2.map_snd ~f:(fun x -> Option.value_exn x) in
            let m = Float.(max max_err (i - q |> abs)) in
            Float.(i + 0.01), m, td
          ) (0.01, 0.0, td)
        in
        if Float.(max_err >= 0.01) then failwith "max_err >= 0.01"
      );

      "from an exact match", `Quick, (fun () ->
        let td =
          Tdigest.create ~delta:(Tdigest.Compress 0.001) ~k:0 ()
          |> Fn.apply_n_times ~n:10 (Tdigest.add_list [10.; 20.; 30.])
        in
        let _td, q = Tdigest.p_rank td 20. in
        if Float.((Option.value_exn q) <> 0.5) then failwith "p_rank(20) <> 0.5"
      );

    ];
    "percentiles", [

      "reports None when given no points", `Quick, (fun () ->
        let td = Tdigest.create () in
        if Option.is_some (Tdigest.percentile td 0.5 |> snd) then failwith "percentile of empty should be None"
      );

      "from a single point", `Quick, (fun () ->
        let td =
          Tdigest.create ()
          |> Tdigest.add ~mean:0.
        in
        check_percentiles td
          [0.; 0.5; 1.]
          [0.; 0.; 0.]
      );

      "from two points", `Quick, (fun () ->
        let td =
          Tdigest.create ()
          |> Tdigest.add_list [0.; 1.]
        in
        check_percentiles td
          [(-1//4); 0.; 1//4; 1//2; 5//8; 3//4; 1.; 1.25]
          [0.; 0.; 0.; 0.5; 0.75; 1.; 1.; 1.]
      );

      "from three points", `Quick, (fun () ->
        let td =
          Tdigest.create ()
          |> Tdigest.add_list [0.; 0.5; 1.]
        in
        check_percentiles td
          [0.; 1//4; 1//2; 3//4; 1.]
          [0.; 0.125; 0.5; 0.875; 1.]
      );

      "from four points", `Quick, (fun () ->
        let td =
          Tdigest.create ()
          |> Tdigest.add_list [10.; 11.; 12.; 13.]
        in
        check_percentiles td
          [0.; 1//4; 1//2; 3//4; 1.]
          [10.; 10.5; 11.5; 12.5; 13.]
      );

      "from lots of uniformly distributed points", `Quick, (fun () ->
        let td =
          Tdigest.create ()
          |> Fn.apply_n_times
            ~n:100_000
            (fun td -> Tdigest.add td ~mean:(Random.float 1.))
          |> Tdigest.Testing.compress
        in
        let _i, max_err, _td =
          Fn.apply_n_times ~n:100 (fun (i, max_err, td) ->
            let td, q = Tdigest.p_rank td i |> Tuple2.map_snd ~f:(fun x -> Option.value_exn x) in
            let m = Float.(max max_err (i - q |> abs)) in
            Float.(i + 0.01), m, td
          ) (0.01, 0.0, td)
        in
        if Float.(max_err >= 0.01) then failwith "max_err >= 0.01"
      );

    ]
  ]
