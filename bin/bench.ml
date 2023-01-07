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

let snapshot sf t0 t1 =
  print_endline (sprintf sf Int63.((t1 - t0) / of_int 1_000 |> to_string_hum ~delimiter:','))

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

  snapshot "Add 100k: %s us" t0 t1;
  print_endline (info_to_string info1);
  snapshot "Compress: %s us" t1 t2;
  print_endline (info_to_string info2);

  let t3 = Time_now.nanoseconds_since_unix_epoch () in
  let _td, str = Tdigest.to_string td in
  let t4 = Time_now.nanoseconds_since_unix_epoch () in
  print_endline (sprintf "Serialized into %d bytes" (String.length str));
  snapshot "Serialization: %s us" t3 t4;

  let t5 = Time_now.nanoseconds_since_unix_epoch () in
  let td = Tdigest.of_string (sprintf "%s%s" str str) in
  let t6 = Time_now.nanoseconds_since_unix_epoch () in
  snapshot "Parsing: %s us" t5 t6;
  let info3 = Tdigest.info td in
  print_endline (info_to_string info3)
