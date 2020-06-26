open! Core_kernel

let () =
  let td = Tdigest.create () in
  let t0 = Time_now.nanoseconds_since_unix_epoch () in
  let td = Fn.apply_n_times
      ~n:100_000
      (fun td -> Tdigest.add td ~mean:(Random.float 1.))
      td
  in
  let t1 = Time_now.nanoseconds_since_unix_epoch () in
  let size1 = Tdigest.Testing.size td in
  let td = Tdigest.Testing.compress td in
  let t2 = Time_now.nanoseconds_since_unix_epoch () in
  let size2 = Tdigest.Testing.size td in
  print_endline (sprintf "Add 100k: %s" Int63.((t1 - t0) / (of_int 1_000_000) |> to_string));
  print_endline (sprintf "Compress: %s" Int63.((t2 - t1) / (of_int 1_000_000) |> to_string));
  print_endline (sprintf "Size: %d -> %d" size1 size2);
