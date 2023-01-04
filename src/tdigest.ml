open! Core
open Float
open Option.Monad_infix

type delta =
  | Merging  of float
  | Discrete

type k =
  | Manual
  | Automatic of float

type cx =
  | Always
  | Growth of float

let default_delta = Merging 0.01

let default_k = Automatic 25.0

let default_cx = Growth 1.1

type settings = {
  delta: delta;
  k: k;
  cx: cx;
}

type centroid = {
  mean: float;
  cumn: float;
  mean_cumn: float;
  n: float;
}

let empty_centroid = { mean = 0.0; n = 0.0; cumn = 0.0; mean_cumn = 0.0 }

type stats = {
  cumulates_count: int;
  compress_count: int;
  auto_compress_count: int;
}

let empty_stats = { cumulates_count = 0; compress_count = 0; auto_compress_count = 0 }

type t = {
  settings: settings;
  centroids: centroid Map.t;
  mutable min: centroid option;
  mutable max: centroid option;
  n: float;
  last_cumulate: float;
  stats: stats;
}

type info = {
  count: int;
  size: int;
  cumulates_count: int;
  compress_count: int;
  auto_compress_count: int;
}
[@@deriving sexp]

type bounds =
  | Neither
  | Both    of centroid * centroid
  | Equal   of centroid
  | Lower   of centroid
  | Upper   of centroid

let get_min = function
| { min = Some _ as x; _ } -> x
| { min = None; n = 0.0; _ } -> None
| { min = None; _ } as td ->
  let min = Map.min_elt td.centroids >>| snd in
  td.min <- min;
  min

let get_max = function
| { max = Some _ as x; _ } -> x
| { max = None; n = 0.0; _ } -> None
| { max = None; _ } as td ->
  let max = Map.max_elt td.centroids >>| snd in
  td.max <- max;
  max

let create ?(delta = default_delta) ?(k = default_k) ?(cx = default_cx) () =
  let k =
    match k with
    | Manual -> k
    | Automatic x when is_positive x -> k
    | Automatic 0.0 ->
      invalid_arg
        "TDigest k parameter cannot be zero, set to Tdigest.Manual to disable automatic compression."
    | Automatic x -> invalid_argf "TDigest k parameter must be positive, but was %f" x ()
  in
  let cx =
    match cx with
    | Always -> cx
    | Growth x when is_positive x -> cx
    | Growth 0.0 ->
      invalid_arg
        "TDigest cx parameter cannot be zero, set to Tdigest.Always to disable caching of cumulative \
         totals."
    | Growth x -> invalid_argf "TDigest cx parameter must be positive, but was %f" x ()
  in
  {
    settings = { delta; k; cx };
    centroids = Map.empty;
    min = None;
    max = None;
    n = 0.0;
    last_cumulate = 0.0;
    stats = empty_stats;
  }

let is_empty = function
| { n = 0.0; _ } -> true
| _ -> false

let info { centroids; n; stats; _ } =
  {
    count = to_int n;
    size = Map.length centroids;
    cumulates_count = stats.cumulates_count;
    compress_count = stats.compress_count;
    auto_compress_count = stats.auto_compress_count;
  }

let find_nearest td mean =
  let gt = ref None in
  let lte =
    Map.binary_search td.centroids `Last_less_than_or_equal_to mean ~compare:(fun ~key ~data against ->
        let x = compare key against in
        if Int.is_positive x then gt := Some (key, data);
        x)
  in
  match lte with
  | Some (k, v) when mean = k -> Some v
  | Some (k1, v1) -> (
    match !gt with
    | None -> None
    | Some (k2, _v2) when mean - k1 < k2 - mean -> Some v1
    | Some (_k2, v2) -> Some v2)
  | None -> None

let use_cache = function
| { n; last_cumulate; settings = { cx = Growth cx; _ }; _ } when cx > n / last_cumulate -> true
| _ -> false

let cumulate td ~exact =
  if td.n = td.last_cumulate || ((not exact) && use_cache td)
  then td
  else (
    let cumn = ref 0.0 in
    let centroids =
      Map.map td.centroids ~f:(fun data ->
          let updated = { data with mean_cumn = !cumn + (data.n / 2.); cumn = !cumn + data.n } in
          cumn := updated.cumn;
          updated)
    in
    {
      td with
      centroids;
      min = None;
      max = None;
      n = !cumn;
      last_cumulate = !cumn;
      stats = { td.stats with cumulates_count = succ td.stats.cumulates_count };
    })

let new_bounds ({ mean; _ } as added) = function
| { n = 0.0; min = None; max = None; _ } -> Some added, Some added
| { min = Some existing; max; _ } when mean < existing.mean -> Some added, max
| { min; max = Some existing; _ } when mean > existing.mean -> min, Some added
| { min; max; _ } -> min, max

let new_centroid td ~mean ~n ~cumn =
  let data = { mean; cumn; n; mean_cumn = n / 2. } in
  let centroids = Map.add_exn td.centroids ~key:mean ~data in
  let min, max = new_bounds data td in
  { td with centroids; min; max; n = td.n + n }

let add_weight td nearest ~mean ~n =
  let updated =
    {
      mean =
        (if nearest.mean = mean
        then nearest.mean
        else nearest.mean + (n * (mean - nearest.mean) / (nearest.n + n)));
      cumn = nearest.cumn + n;
      mean_cumn = nearest.mean_cumn + (n / 2.);
      n = nearest.n + n;
    }
  in
  let centroids = Map.remove td.centroids nearest.mean |> Map.add_exn ~key:updated.mean ~data:updated in
  { td with centroids; n = td.n + n; min = None; max = None }

let internal_digest td ~n ~mean =
  let nearest_is_boundary boundary nearest =
    Option.value_map boundary ~default:false ~f:(fun { mean; _ } -> mean = nearest.mean)
  in
  let td =
    match find_nearest td mean, td.settings.delta with
    | Some nearest, _ when nearest.mean = mean -> add_weight td nearest ~mean ~n
    | Some nearest, _ when nearest_is_boundary (get_min td) nearest -> new_centroid td ~mean ~n ~cumn:0.0
    | Some nearest, _ when nearest_is_boundary (get_max td) nearest -> new_centroid td ~mean ~n ~cumn:td.n
    | Some nearest, Discrete -> new_centroid td ~mean ~n ~cumn:nearest.cumn
    | Some nearest, Merging delta ->
      let p = nearest.mean_cumn / td.n in
      let max_n = round_down (4.0 * td.n * delta * p * (1.0 - p)) in
      if max_n - nearest.n >= n
      then add_weight td nearest ~mean ~n
      else new_centroid td ~mean ~n ~cumn:nearest.cumn
    | None, _ -> new_centroid td ~mean ~n ~cumn:0.0
  in
  cumulate td ~exact:false

let shuffle_inplace arr =
  let _i =
    Array.fold_right arr ~init:(Array.length arr) ~f:(fun _x i ->
        let random = Random.float (of_int i) |> to_int in
        let current = pred i in
        Array.swap arr random current;
        current)
  in
  ()

let weights_of_td = function
(* n is out of sync, must check centroids *)
| { centroids; _ } when Map.is_empty centroids -> [||]
| { centroids; _ } ->
  let arr = Array.create ~len:(Map.length centroids) empty_centroid in
  let _i =
    Map.fold centroids ~init:0 ~f:(fun ~key:_ ~data i ->
        Array.set arr i data;
        succ i)
  in
  arr

let weights_of_map ~len map =
  let arr = Array.create ~len empty_centroid in
  let _i =
    Map.fold map ~init:0 ~f:(fun ~key:mean ~data:n i ->
        Array.set arr i { empty_centroid with mean; n };
        succ i)
  in
  arr

let rebuild ~auto settings (stats : stats) arr =
  shuffle_inplace arr;
  let blank =
    {
      settings;
      centroids = Map.empty;
      min = None;
      max = None;
      n = 0.0;
      last_cumulate = 0.0;
      stats =
        {
          stats with
          compress_count = succ stats.compress_count;
          auto_compress_count = (if auto then succ else Fn.id) stats.auto_compress_count;
        };
    }
  in
  let td = Array.fold arr ~init:blank ~f:(fun acc { mean; n; _ } -> internal_digest acc ~n ~mean) in
  cumulate td ~exact:true

let digest ?(n = 1) td ~mean =
  let td = internal_digest td ~n:(Int.to_float n) ~mean in
  match td.settings with
  | { delta = Merging delta; k = Automatic k; _ } when Map.length td.centroids |> of_int > k / delta ->
    rebuild ~auto:true td.settings td.stats (weights_of_td td)
  | _ -> td

let compress ?delta td =
  match delta with
  | None -> rebuild ~auto:false td.settings td.stats (weights_of_td td)
  | Some delta ->
    let settings = td.settings in
    let updated = rebuild ~auto:false { td.settings with delta } td.stats (weights_of_td td) in
    { updated with settings }

let add ?(n = 1) ~data td = digest td ~n ~mean:data

let add_list ?(n = 1) xs td = List.fold xs ~init:td ~f:(fun acc mean -> digest acc ~n ~mean)

let to_string td =
  let buf = Buffer.create (Map.length td.centroids |> Int.( * ) 16) in
  let add_float f =
    let v = Int64.bits_of_float f in
    let rec loop = function
      | 8 -> ()
      | i ->
        Buffer.add_char buf Int64.(255L land shift_right v Int.(i * 8) |> to_int_exn |> Char.of_int_exn);
        (loop [@tailcall]) (succ i)
    in
    loop 0
  in
  Map.iter td.centroids ~f:(fun { mean; n; _ } ->
      add_float mean;
      add_float n);
  td, Buffer.contents buf

let of_string ?(delta = default_delta) ?(k = default_k) ?(cx = default_cx) str =
  if Int.(String.length str % 16 <> 0) then invalid_arg "Invalid string length for Tdigest.of_string";
  let settings = { delta; k; cx } in
  let _i, _mean, _n, map =
    String.fold str ~init:(0, 0L, 0L, Map.empty) ~f:(fun (i, pmean, pn, acc) c ->
        let x = c |> Char.to_int |> Int64.of_int_exn in
        match i with
        | 0
         |1
         |2
         |3
         |4
         |5
         |6
         |7 ->
          let mean = Int64.(pmean lor shift_left x Int.(i * 8)) in
          succ i, mean, pn, acc
        | 8
         |9
         |10
         |11
         |12
         |13
         |14 ->
          let n = Int64.(pn lor shift_left x Int.((i - 8) * 8)) in
          succ i, pmean, n, acc
        | 15 ->
          let mean = Int64.float_of_bits pmean in
          let n = Int64.(pn lor shift_left x 56 |> float_of_bits) in
          let acc = Map.update acc mean ~f:(Option.value_map ~default:n ~f:(( + ) n)) in
          0, 0L, 0L, acc
        | x -> failwithf "Tdigest.of_string: impossible case '%d'. Please report this bug." x ())
  in
  weights_of_map ~len:Int.(String.length str / 16) map |> rebuild ~auto:true settings empty_stats

let merge ?(delta = default_delta) ?(k = default_k) ?(cx = default_cx) tds =
  let settings = { delta; k; cx } in
  let map =
    List.fold tds ~init:Map.empty ~f:(fun acc { centroids; _ } ->
        Map.fold centroids ~init:acc ~f:(fun ~key:_ ~data:{ mean; n; _ } acc ->
            Map.update acc mean ~f:(Option.value_map ~default:n ~f:(( + ) n))))
  in
  weights_of_map ~len:(Map.length map) map |> rebuild ~auto:true settings empty_stats

let bounds td needle lens =
  let gt = ref None in
  let lte =
    Map.binary_search td.centroids `Last_less_than_or_equal_to needle ~compare:(fun ~key ~data against ->
        let x = compare (lens data) against in
        if Int.is_positive x then gt := Some (key, data);
        x)
  in
  match lte with
  | Some (_k, v) when lens v = needle -> Equal v
  | Some (_k1, v1) -> (
    match !gt with
    | Some (_k2, v2) -> Both (v1, v2)
    | None -> Lower v1)
  | None -> (
    match get_min td with
    | Some v -> Upper v
    | None -> Neither)

let percentile td p =
  match td with
  | { n = 0.0; _ } -> td, None
  | td -> (
    let td = cumulate td ~exact:true in
    let h = td.n * p in
    match bounds td h (fun { mean_cumn; _ } -> mean_cumn), td.settings.delta with
    | Lower x, _
     |Upper x, _
     |Equal x, _ ->
      td, Some x.mean
    | Both (lower, upper), Merging _ ->
      let num =
        lower.mean
        + ((h - lower.mean_cumn) * (upper.mean - lower.mean) / (upper.mean_cumn - lower.mean_cumn))
      in
      td, Some num
    | Both (lower, _upper), Discrete when h <= lower.cumn -> td, Some lower.mean
    | Both (_lower, upper), Discrete -> td, Some upper.mean
    | Neither, _ -> td, None)

let percentiles td ps = List.fold_map ps ~init:td ~f:percentile

let p_rank td p =
  match get_min td with
  | None -> td, None
  | Some v when p < v.mean -> td, Some 0.0
  | Some _ -> (
    match get_max td with
    | None -> td, None
    | Some v when p > v.mean -> td, Some 1.0
    | Some _ -> (
      let td = cumulate td ~exact:true in
      match bounds td p (fun { mean; _ } -> mean), td.settings.delta with
      | Both (lower, _), Discrete
       |Lower lower, Discrete
       |Equal lower, Discrete ->
        td, Some (lower.cumn / td.n)
      | Neither, Discrete
       |Upper _, Discrete ->
        td, None
      | Equal x, Merging _ -> td, Some (x.mean_cumn / td.n)
      | Both (lower, upper), Merging _ ->
        let num =
          lower.mean_cumn
          + ((p - lower.mean) * (upper.mean_cumn - lower.mean_cumn) / (upper.mean - lower.mean))
        in
        td, Some (num / td.n)
      | _, Merging _ -> td, None))

let p_ranks td ps = List.fold_map ps ~init:td ~f:p_rank

module Private = struct
  let to_yojson td =
    let ll =
      Map.fold_right td.centroids ~init:[] ~f:(fun ~key:_ ~data acc ->
          `Assoc [ "mean", `Float data.mean; "n", `Float data.n ] :: acc)
    in
    `Assoc [ "centroids", `List ll ]

  let min td = get_min td >>| fun { mean; n; _ } -> mean, n

  let max td = get_max td >>| fun { mean; n; _ } -> mean, n
end
