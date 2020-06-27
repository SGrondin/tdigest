open! Core_kernel

let () =
  let arr = Array.init 1_000_000 ~f:(fun _i -> Random.float 1.) in
  let td = Tdigest.create ~compression:Tdigest.Manual () in

  let t0 = Time_now.nanoseconds_since_unix_epoch () in
  let td = Array.fold arr ~init:td ~f:(fun acc mean -> Tdigest.add acc ~mean) in

  let t1 = Time_now.nanoseconds_since_unix_epoch () in
  let size1 = (Tdigest.info td).size in
  let td = Tdigest.compress td in

  let t2 = Time_now.nanoseconds_since_unix_epoch () in
  let size2 = (Tdigest.info td).size in

  print_endline (sprintf "Add 100k: %sms" Int63.((t1 - t0) / (of_int 1_000_000) |> to_string));
  print_endline (sprintf "Compress: %sms" Int63.((t2 - t1) / (of_int 1_000_000) |> to_string));
  print_endline (sprintf "Size: %d -> %d" size1 size2);
