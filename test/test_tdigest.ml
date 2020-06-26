open! Core_kernel

open Shared

let () =
  Alcotest.run "TDigest" [
    "T-Digests in which each point becomes a centroid", [

      "consumes a point", `Quick, (fun () ->
        let td = Tdigest.create () in
        Tdigest.add td 0.;
        check td [b 0. 1]
      );

      "consumes two points", `Quick, (fun () ->
        let td = Tdigest.create () in
        Tdigest.add_list td [0.; 1.];
        check td [b 0. 1; b 1. 1]
      );

      "consumes three points", `Quick, (fun () ->
        let td = Tdigest.create () in
        Tdigest.add_list td [0.; 1.; -1.];
        check td [b (-1.) 1; b 0. 1; b 1. 1]
      );

      "consumes increasing-valued points", `Quick, (fun () ->
        let td = Tdigest.create ~delta:(Tdigest.Compress 0.001) ~k:0 () in
        List.init 100 ~f:(fun i -> i * 10 |> Float.of_int)
        |> Tdigest.add_list td;
        let against = List.init 100 ~f:(fun i -> b (i * 10 |> Float.of_int) 1) in
        check td against
      );

      "consumes decreasing-valued points", `Quick, (fun () ->
        let td = Tdigest.create ~delta:(Tdigest.Compress 0.001) ~k:0 () in
        List.init 100 ~f:(fun i -> (99 - i) * 10 |> Float.of_int)
        |> Tdigest.add_list td;
        let against = List.init 100 ~f:(fun i -> b (i * 10 |> Float.of_int) 1) in
        check td against
      );

    ];
    "T-Digests in which points are merged into centroids", [

      "consumes same-valued points into a single point", `Quick, (fun () ->
        let td = Tdigest.create () in
        Fn.apply_n_times ~n:100 (fun () ->
          Tdigest.add td 1000.
        ) ();
        check td [b 1000. 100]
      );

      "handles multiple duplicates", `Quick, (fun () ->
        let td = Tdigest.create ~delta:(Tdigest.Compress 1.) ~k:0 ~cx:0. () in
        Fn.apply_n_times ~n:10 (fun () ->
          Tdigest.add td 0.;
          Tdigest.add td 1.;
          Tdigest.add td 0.5
        ) ();
        check td [b 0. 10; b 0.5 10; b 1. 10]
      );

    ];
    "compress", [

      "compresses points and preserves bounds", `Quick, (fun () ->
        let td = Tdigest.create ~delta:(Tdigest.Compress 0.001) ~k:0 () in
        List.init 100 ~f:(fun i -> i * 10 |> Float.of_int)
        |> Tdigest.add_list td;

        let size = Tdigest.Testing.size td in
        if size <> 100 then failwithf "Original size: %d <> 100" size ();

        Tdigest.Testing.compress_with_delta td (Tdigest.Compress 0.1);
        let size = Tdigest.Testing.size td in
        if size >= 100 then failwithf "Compressed size: %d >= 100" size ();

        Json_diff.assert_equal (Tdigest.Testing.min td) (b 0. 1 |> basic_to_yojson);
        Json_diff.assert_equal (Tdigest.Testing.max td) (b 990. 1 |> basic_to_yojson)
      );

      "K automatically compresses during ingest", `Quick, (fun () ->
        let td = Tdigest.create () in
        Array.init 10_000 ~f:(fun i -> i * 10 |> Float.of_int)
        |> Array.iter ~f:(Tdigest.add td);

        let size = Tdigest.Testing.size td in
        if size >= 10_000 then failwithf "Size: %d >= 10,000" size ();

        Json_diff.assert_equal (Tdigest.Testing.min td) (b 0. 1 |> basic_to_yojson);
        Json_diff.assert_equal (Tdigest.Testing.max td) (b 99_990. 1 |> basic_to_yojson)
      );

    ];

    "percentile ranks", [

      "reports None when given no points", `Quick, (fun () ->
        let td = Tdigest.create () in
        if Option.is_some (Tdigest.p_rank td 1.) then failwith "p_rank of empty should be None"
      );

      "from a single point", `Quick, (fun () ->
        let td = Tdigest.create () in
        Tdigest.add td 0.;
        check_p_ranks td
          [-0.5; 0.; 0.5; 1.; 1.5]
          [0.; 0.5; 1.; 1.; 1.]
      );

      "from two points", `Quick, (fun () ->
        let td = Tdigest.create () in
        Tdigest.add_list td [0.; 1.];
        check_p_ranks td
          [-0.5; 0.; 0.5; 1.; 1.5]
          [0.; 0.25; 0.5; 0.75; 1.]
      );

      "from three points", `Quick, (fun () ->
        let td = Tdigest.create () in
        Tdigest.add_list td [-1.; 0.; 1.];
        check_p_ranks td
          [-1.5; -1.0; -0.5; 0.; 0.5; 1.0; 1.5]
          [0.; 1//6; 2//6; 3//6; 4//6; 5//6; 1.]
      );

      "from three points is same as from multiples of those points", `Quick, (fun () ->
        let td = Tdigest.create () in
        Tdigest.add_list td [0.; 1.; -1.];
        let result1 = List.map [-1.5; -1.0; -0.5; 0.; 0.5; 1.0; 1.5] ~f:(Tdigest.p_rank td) in
        Tdigest.add_list td [0.; 1.; -1.];
        Tdigest.add_list td [0.; 1.; -1.];
        let result2 = List.map [-1.5; -1.0; -0.5; 0.; 0.5; 1.0; 1.5] ~f:(Tdigest.p_rank td) in
        Json_diff.assert_equal (float_opts_to_yojson result1) (float_opts_to_yojson result2)
      );

      "from four points away from the origin", `Quick, (fun () ->
        let td = Tdigest.create () in
        Tdigest.add_list td [10.; 11.; 12.; 13.];
        check_p_ranks td
          [9.; 10.; 11.; 12.; 13.; 14.]
          [0.; 1//8; 3//8; 5//8; 7//8; 1.]
      );

      "from four points is same as from multiples of those points", `Quick, (fun () ->
        let td = Tdigest.create ~delta:(Tdigest.Compress 0.) ~k:0 () in
        Tdigest.add_list td [10.; 11.; 12.; 13.];
        let result1 = List.map [9.; 10.; 11.; 12.; 13.; 14.] ~f:(Tdigest.p_rank td) in
        Tdigest.add_list td [10.; 11.; 12.; 13.];
        Tdigest.add_list td [10.; 11.; 12.; 13.];
        let result2 = List.map [9.; 10.; 11.; 12.; 13.; 14.] ~f:(Tdigest.p_rank td) in
        Json_diff.assert_equal (float_opts_to_yojson result1) (float_opts_to_yojson result2)
      );

      "from lots of uniformly distributed points", `Quick, (fun () ->
        let td = Tdigest.create () in
        Fn.apply_n_times ~n:100_000 (fun () ->
          Tdigest.add td (Random.float 1.)
        ) ();
        Tdigest.Testing.compress td;
        let _i, max_err = Fn.apply_n_times ~n:100 (fun (i, max_err) ->
            let q = Tdigest.p_rank td i |> Option.value_exn in
            let m = Float.(max max_err (i - q |> abs)) in
            Float.(i + 0.01), m
          ) (0.01, 0.0)
        in
        if Float.(max_err >= 0.01) then failwith "max_err >= 0.01"
      );

      "from an exact match", `Quick, (fun () ->
        let td = Tdigest.create ~delta:(Tdigest.Compress 0.001) ~k:0 () in
        Fn.apply_n_times ~n:10 (fun () ->
          Tdigest.add_list td [10.; 20.; 30.]
        ) ();
        if Float.((Tdigest.p_rank td 20. |> Option.value_exn) <> 0.5) then failwith "p_rank(20) <> 0.5"
      );

    ];
    "percentiles", [

      "reports None when given no points", `Quick, (fun () ->
        let td = Tdigest.create () in
        if Option.is_some (Tdigest.percentile td 0.5) then failwith "percentile of empty should be None"
      );

      "from a single point", `Quick, (fun () ->
        let td = Tdigest.create () in
        Tdigest.add td 0.;
        check_percentiles td
          [0.; 0.5; 1.]
          [0.; 0.; 0.]
      );

      "from two points", `Quick, (fun () ->
        let td = Tdigest.create () in
        Tdigest.add_list td [0.; 1.];
        check_percentiles td
          [(-1//4); 0.; 1//4; 1//2; 5//8; 3//4; 1.; 1.25]
          [0.; 0.; 0.; 0.5; 0.75; 1.; 1.; 1.]
      );

      "from three points", `Quick, (fun () ->
        let td = Tdigest.create () in
        Tdigest.add_list td [0.; 0.5; 1.];
        check_percentiles td
          [0.; 1//4; 1//2; 3//4; 1.]
          [0.; 0.125; 0.5; 0.875; 1.]
      );

      "from four points", `Quick, (fun () ->
        let td = Tdigest.create () in
        Tdigest.add_list td [10.; 11.; 12.; 13.];
        check_percentiles td
          [0.; 1//4; 1//2; 3//4; 1.]
          [10.; 10.5; 11.5; 12.5; 13.]
      );

      "from lots of uniformly distributed points", `Quick, (fun () ->
        let td = Tdigest.create () in
        Fn.apply_n_times ~n:100_000 (fun () ->
          Tdigest.add td (Random.float 1.)
        ) ();
        Tdigest.Testing.compress td;
        let _i, max_err = Fn.apply_n_times ~n:100 (fun (i, max_err) ->
            let q = Tdigest.percentile td i |> Option.value_exn in
            let m = Float.(max max_err (i - q |> abs)) in
            Float.(i + 0.01), m
          ) (0.01, 0.0)
        in
        if Float.(max_err >= 0.01) then failwith "max_err >= 0.01"
      );

    ]
  ]
