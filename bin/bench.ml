open! Core

let info_to_string x =
  let open Tdigest in
  sprintf {s|-----
Count: %d
Size: %d
# Cumulates: %d
# Compressions: %d
# Auto Compressions: %d
-----|s}
    x.count x.size x.cumulates_count x.compress_count x.auto_compress_count

let () =
  let arr = Array.init 2_000_000 ~f:(fun _i -> Random.float 1.) in
  let td = Tdigest.create () in

  let t0 = Time_now.nanoseconds_since_unix_epoch () in
  let td = Array.fold arr ~init:td ~f:(fun acc data -> Tdigest.add acc ~data) in

  let t1 = Time_now.nanoseconds_since_unix_epoch () in
  let info1 = Tdigest.info td in
  let td = Tdigest.compress td in

  let t2 = Time_now.nanoseconds_since_unix_epoch () in
  let info2 = Tdigest.info td in

  print_endline (sprintf "Add 100k: %sms" Int63.((t1 - t0) / of_int 1_000_000 |> to_string));
  print_endline (info_to_string info1);
  print_endline (sprintf "Compress: %sms" Int63.((t2 - t1) / of_int 1_000_000 |> to_string));
  print_endline (info_to_string info2);

  let t3 = Time_now.nanoseconds_since_unix_epoch () in
  let _td, str = Tdigest.to_string td in
  let t4 = Time_now.nanoseconds_since_unix_epoch () in
  print_endline (sprintf "Serialized into %d bytes" (String.length str));
  print_endline (sprintf "Serialization: %sms" Int63.((t4 - t3) / of_int 1_000_000 |> to_string));

  let t5 = Time_now.nanoseconds_since_unix_epoch () in
  let td = Tdigest.of_string (sprintf "%s%s" str str) in
  let t6 = Time_now.nanoseconds_since_unix_epoch () in
  print_endline (sprintf "Parsing: %sms" Int63.((t6 - t5) / of_int 1_000_000 |> to_string));
  let info3 = Tdigest.info td in
  print_endline (info_to_string info3)
