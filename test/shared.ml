open! Core_kernel

type basic = {
  mean: float;
  n: float;
} [@@deriving fields]

let b mean n = { mean; n = Int.to_float n }

let basic_to_yojson { mean; n } : Yojson.Safe.t = `Assoc ["mean", `Float mean; "n", `Float n]

let floats_to_yojson ll = `Assoc ["value", `List (List.map ll ~f:(fun x -> `Float x))]

let float_opts_to_yojson ll = `Assoc [
    "value", `List (List.map ll ~f:(function
      | Some x -> `Float x
      | None -> `Null
      ))]

let check_fn td queries results ~fn =
  let _td, ranks = fn td queries in
  let left = float_opts_to_yojson ranks in
  let right = floats_to_yojson results in
  Json_diff.assert_equal left right

let check_p_ranks = check_fn ~fn:Tdigest.p_ranks

let check_percentiles = check_fn ~fn:Tdigest.percentiles

let check td vs =
  let json : Yojson.Safe.t = Tdigest.Testing.to_yojson td true in
  let against =
    `Assoc [
      "centroids", `List (List.map vs ~f:basic_to_yojson)
    ]
  in
  Json_diff.assert_equal json against
