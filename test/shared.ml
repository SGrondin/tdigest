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

let check_fn td queries results fn =
  let left = List.map queries ~f:(fn td) |> float_opts_to_yojson in
  let right = floats_to_yojson results in
  Json_diff.assert_equal left right

let check_p_ranks td queries results = check_fn td queries results Tdigest.p_rank
let check_percentiles td queries results = check_fn td queries results Tdigest.percentile

let check td vs =
  let json : Yojson.Safe.t = Tdigest.Testing.to_yojson td true in
  let against =
    `Assoc [
      "centroids", `List (List.map vs ~f:basic_to_yojson)
    ]
  in
  Json_diff.assert_equal json against
