open! Core

type mismatch =
  | Changed of (Yojson.Safe.t * Yojson.Safe.t)
  | Added   of Yojson.Safe.t
  | Deleted of Yojson.Safe.t

let color s = function
| `Yellow -> sprintf "\x1b[0;33m%s\x1b[m" s
| `Green -> sprintf "\x1b[0;32m%s\x1b[m" s
| `Red -> sprintf "\x1b[0;31m%s\x1b[m" s

let flatten (json : Yojson.Safe.t) =
  let rec loop json prefix acc =
    match json with
    | `Assoc pairs ->
      String.Table.add_exn acc ~key:prefix ~data:(`Assoc []);
      List.iter pairs ~f:(fun (key, value) -> loop value (sprintf "%s.%s" prefix key) acc)
    | `List values ->
      String.Table.add_exn acc ~key:prefix ~data:(`List []);
      List.iteri values ~f:(fun key value -> loop value (sprintf "%s[%d]" prefix key) acc)
    | x -> String.Table.add_exn acc ~key:prefix ~data:x
  in
  match json with
  | `Assoc _ ->
    let acc = String.Table.create () in
    loop json "" acc;
    acc
  | x -> failwithf "Cannot flatten a non-object: %s" (Yojson.Safe.to_string x) ()

let assert_equal left right =
  let errors = Queue.create () in
  let mismatch s =
    Queue.enqueue errors s;
    None
  in
  let stringify = Yojson.Safe.to_string in
  let _merged =
    String.Table.merge (flatten left) (flatten right) ~f:(fun ~key -> function
      | `Left x -> sprintf "%s: %s" (color (sprintf "+++ %s" key) `Green) (stringify x) |> mismatch
      | `Right y -> sprintf "%s: %s" (color (sprintf "--- %s" key) `Red) (stringify y) |> mismatch
      | `Both (x, y) when Yojson.Safe.equal x y -> None
      | `Both (x, y) ->
        sprintf "%s: %s %s %s"
          (color (sprintf "+/- %s" key) `Yellow)
          (stringify x)
          (color "!=" `Yellow)
          (stringify y)
        |> mismatch)
  in
  match Queue.length errors with
  | 0 -> ()
  | n ->
    printf "%s:\n%s\n" (color "Actual" `Green) (Yojson.Safe.pretty_to_string left);
    eprintf "JSON Mismatch (%d errors):\n%s\n" n (Queue.to_array errors |> String.concat_array ~sep:"\n");
    failwith "JSON Mismatch"
