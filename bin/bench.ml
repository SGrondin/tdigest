open! Base

let print_endline = Stdlib.print_endline

let sprintf = Printf.sprintf

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
  print_endline
    (sprintf sf Float.((t1 - t0) * 1_000_000. |> round_nearest |> to_string_hum ~delimiter:','))

module type S = module type of Tdigest.M

let test (module M : S) =
  let arr = Array.init 2_000_000 ~f:(fun _i -> Random.float 1.) in
  let td = M.create () in

  let t0 = Unix.gettimeofday () in
  let td = Array.fold arr ~init:td ~f:(fun acc data -> M.add acc ~data) in

  let t1 = Unix.gettimeofday () in
  let info1 = M.info td in
  let td = M.compress td in

  let t2 = Unix.gettimeofday () in
  let info2 = M.info td in

  snapshot "Add 2_000_000: %s us" t0 t1;
  print_endline (info_to_string info1);
  snapshot "Compress: %s us" t1 t2;
  print_endline (info_to_string info2);

  let t3 = Unix.gettimeofday () in
  let _td, str = M.to_string td in
  let t4 = Unix.gettimeofday () in
  print_endline (sprintf "Serialized into %d bytes" (String.length str));
  snapshot "Serialization: %s us" t3 t4;

  let t5 = Unix.gettimeofday () in
  let td = M.of_string (sprintf "%s%s" str str) in
  let t6 = Unix.gettimeofday () in
  snapshot "Parsing: %s us" t5 t6;
  let info3 = M.info td in
  print_endline (info_to_string info3)

let () = test (module Tdigest)

let () = test (module Tdigest.Marshallable)
