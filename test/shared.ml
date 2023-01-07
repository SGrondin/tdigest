open! Core

let render_centroids ll =
  List.map ll ~f:(fun (mean, n) -> ("mean", mean), ("n", n))
  |> [%sexp_of: ((string * float) * (string * float)) list]

let check td = Tdigest.Private.centroids td |> render_centroids |> Sexp.to_string_hum |> print_endline

let check_size td = (Tdigest.info td).size |> sprintf !"%{sexp: int}" |> print_endline

let check_min_max td =
  (Tdigest.Private.min td, Tdigest.Private.max td)
  |> sprintf !"%{sexp: (float * float) option * (float * float) option}"
  |> print_endline

let check_p_rank p td = Tdigest.p_rank td p |> snd |> sprintf !"%{sexp: float option}" |> print_endline

let check_percentile p td =
  Tdigest.percentile td p |> snd |> sprintf !"%{sexp: float option}" |> print_endline

let check_p_ranks ps td =
  Tdigest.p_ranks td ps |> snd |> sprintf !"%{sexp: float option list}" |> print_endline

let check_percentiles ps td =
  Tdigest.percentiles td ps |> snd |> sprintf !"%{sexp: float option list}" |> print_endline

let identical_sexp ~received ~expected =
  if not (Sexp.equal received expected)
  then print_endline (sprintf !"Not identical.\nReceived: %{Sexp}\nExpected: %{Sexp}" received expected)
  else print_endline "Identical"

let expected_centroids td =
  let received = Tdigest.Private.centroids td |> render_centroids in
  let expected = List.init 100 ~f:(fun i -> i * 10 |> Int.to_float, 1.) |> render_centroids in
  identical_sexp ~received ~expected
