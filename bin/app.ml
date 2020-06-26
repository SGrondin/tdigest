open! Core_kernel

let () =
  let td = Tdigest.create () in
  Tdigest.add_list td [0.; 1.];
  let json = Tdigest.Testing.to_yojson td false in
  print_endline (Yojson.Safe.pretty_to_string json);
  print_endline (sprintf "%f" (1//4));
  print_endline (Tdigest.percentile td (1//4) |> Option.value_map ~f:Float.to_string ~default:"---");
