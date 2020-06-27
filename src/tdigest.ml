open! Core_kernel
open Float

type delta =
| Merging of float
| Discrete

type k =
| Manual
| Automatic of float

type cx =
| Always
| Growth of float

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

type stats = {
  cumulates_count: int;
  compress_count: int;
  auto_compress_count: int;
}

type t = {
  settings: settings;
  centroids: centroid Float.Map.t;
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

type bounds =
| Neither
| Both of (centroid * centroid)
| Equal of centroid
| Lower of centroid
| Upper of centroid

let create ?(delta = Merging 0.01) ?(compression = Automatic 25.) ?(refresh = Growth 1.1) () =
  let k = begin match compression with
  | Manual -> compression
  | Automatic x when Float.is_positive x -> compression
  | Automatic 0. -> failwith "TDigest compression k parameter cannot be zero, set to Tdigest.Manual to disable automatic compression."
  | Automatic x -> failwithf "TDigest compression k parameter must be positive, but was %f" x ()
  end
  in
  let cx = begin match refresh with
  | Always -> refresh
  | Growth x when Float.is_positive x -> refresh
  | Growth 0. -> failwith "TDigest refresh cx parameter cannot be zero, set to Tdigest.Always to disable caching of cumulative totals."
  | Growth x -> failwithf "TDigest refresh cx parameter must be positive, but was %f" x ()
  end
  in
  {
    settings = {
      delta;
      k;
      cx;
    };
    centroids = Float.Map.empty;
    n = 0.;
    last_cumulate = 0.;
    stats = {
      cumulates_count = 0;
      compress_count = 0;
      auto_compress_count = 0;
    };
  }

let info { centroids; n; stats; _ } =
  {
    count = to_int n;
    size = Float.Map.length centroids;
    cumulates_count = stats.cumulates_count;
    compress_count = stats.compress_count;
    auto_compress_count = stats.auto_compress_count;
  }

let find_nearest td mean =
  begin match Float.Map.closest_key td.centroids `Less_or_equal_to mean with
  | Some (k, v) when mean = k -> Some v
  | Some (k1, v1) ->
    begin match Float.Map.closest_key td.centroids `Greater_than mean with
    | None -> None
    | Some (k2, _v2) when (mean - k1) < (k2 - mean) -> Some v1
    | Some (_k2, v2) -> Some v2
    end
  | None -> None
  end

let new_centroid td ~mean ~n ~cumn =
  let data = { mean; cumn; n; mean_cumn = n / 2. } in
  let centroids = Float.Map.add_exn td.centroids ~key:data.mean ~data in
  { td with centroids; n = td.n + n }

let add_weight td nearest ~mean ~n =
  let updated = {
    mean =
      if nearest.mean = mean
      then nearest.mean
      else nearest.mean + (n * (mean - nearest.mean) / (nearest.n + n));
    cumn = nearest.cumn + n;
    mean_cumn = nearest.mean_cumn + n / 2.;
    n = nearest.n + n;
  }
  in
  let centroids =
    Float.Map.remove td.centroids nearest.mean
    |> Float.Map.add_exn ~key:updated.mean ~data:updated
  in
  { td with centroids; n = td.n + n }

let use_cache = function
| { n; last_cumulate; settings = { cx = Growth cx; _ }; _ } when cx > n / last_cumulate -> true
| _ -> false

let cumulate td exact =
  if (td.n = td.last_cumulate) || (not exact && use_cache td)
  then td
  else begin
    let cumn, centroids = Float.Map.fold td.centroids ~init:(0., Float.Map.empty) ~f:(fun ~key:_ ~data (cumn, acc) ->
        let updated = { data with
          mean_cumn = cumn + data.n / 2.;
          cumn = cumn + data.n;
        }
        in
        begin match Float.Map.add acc ~key:updated.mean ~data:updated with
        | `Duplicate -> failwith "Cumulate ignored!"
        | `Ok x -> updated.cumn, x
        end
      )
    in
    { td with
      centroids;
      n = cumn;
      last_cumulate = cumn;
      stats = { td.stats with cumulates_count = succ td.stats.cumulates_count }
    }
  end

let internal_digest td ~n ~mean =
  let nearest_is_fn fn nearest =
    Option.value_map ~default:false (fn td.centroids) ~f:(fun (k, _v) -> k = nearest.mean)
  in
  let td = begin match (find_nearest td mean), td.settings.delta with
  | (Some nearest), _ when nearest.mean = mean ->
    add_weight td nearest ~mean ~n
  | (Some nearest), _ when nearest_is_fn Float.Map.min_elt nearest ->
    new_centroid td ~mean ~n ~cumn:0.0
  | (Some nearest), _ when nearest_is_fn Float.Map.max_elt nearest ->
    new_centroid td ~mean ~n ~cumn:td.n
  | (Some nearest), Discrete ->
    new_centroid td ~mean ~n ~cumn:nearest.cumn
  | (Some nearest), Merging delta ->
    let p = nearest.mean_cumn / td.n in
    let max_n = round_down (4.0 * td.n * delta * p * (1.0 - p)) in
    if (max_n - nearest.n) >= n
    then add_weight td nearest ~mean ~n
    else new_centroid td ~mean ~n ~cumn:nearest.cumn
  | None, _ ->
    new_centroid td ~mean ~n ~cumn:0.0
  end
  in
  cumulate td false

let to_array td =
  begin match Float.Map.min_elt td.centroids with
  | None -> [||]
  | Some (_k, v) ->
    let arr = Array.create ~len:(Float.Map.length td.centroids) v in
    let _i = Float.Map.fold td.centroids ~init:0 ~f:(fun ~key:_ ~data i ->
        Array.set arr i data;
        succ i
      )
    in
    arr
  end

let shuffled_array td =
  let arr = to_array td in
  let _i = Array.fold_right arr ~init:(Array.length arr) ~f:(fun _x i ->
      let random = (Random.float 1.0) * (of_int i) |> to_int in
      let current = pred i in
      Array.swap arr random current;
      current
    )
  in
  arr

let rebuild auto td =
  let arr = shuffled_array td in
  let blank = { td with
    centroids = Float.Map.empty;
    n = 0.;
    last_cumulate = 0.;
    stats = { td.stats with
      compress_count = succ td.stats.compress_count;
      auto_compress_count = (if auto then succ else Fn.id) td.stats.auto_compress_count;
    }
  }
  in
  let td = Array.fold arr ~init:blank ~f:(fun acc { mean; n; _ } ->
      internal_digest acc ~n ~mean
    )
  in
  cumulate td true

let digest td ?(n = 1) ~mean =
  let td = internal_digest td ~n:(Int.to_float n) ~mean in
  begin match td.settings with
  | { delta = Merging delta; k = Automatic k; _ } when (Float.Map.length td.centroids |> of_int) > k / delta ->
    rebuild true td
  | _ -> td
  end

let compress ?delta td =
  begin match delta with
  | None -> rebuild false td
  | Some delta ->
    let settings = td.settings in
    let updated = rebuild false { td with settings = { td.settings with delta } } in
    { updated with settings }
  end

let add ?(n = 1) ~mean td = digest td ~n ~mean

let add_list ?(n = 1) xs td = List.fold xs ~init:td ~f:(fun acc mean -> digest acc ~n ~mean)

let bounds td needle lens =
  let search kind =
    Float.Map.binary_search td.centroids kind needle
      ~compare:(fun ~key:_ ~data x -> compare (lens data) x)
  in
  begin match search `Last_less_than_or_equal_to with
  | Some (_k, v) when (lens v) = needle ->
    Equal v
  | Some (_k1, v1) ->
    begin match search `First_strictly_greater_than with
    | Some (_k2, v2) ->
      Both (v1, v2)
    | None ->
      Lower v1
    end
  | None ->
    begin match Float.Map.min_elt td.centroids with
    | Some (_k, v) -> Upper v
    | None -> Neither
    end
  end

let percentile td p =
  if Float.Map.is_empty td.centroids then td, None else begin
    let td = cumulate td true in
    let h = td.n * p in
    begin match (bounds td h (fun { mean_cumn; _ } -> mean_cumn)), td.settings.delta with
    | (Lower x), _
    | (Upper x), _
    | (Equal x), _ -> td, Some x.mean
    | (Both (lower, upper)), Merging _ ->
      let num = lower.mean + (h - lower.mean_cumn) * (upper.mean - lower.mean) / (upper.mean_cumn - lower.mean_cumn) in
      td, Some num
    | (Both (lower, _upper)), Discrete when h <= lower.cumn -> td, Some lower.mean
    | (Both (_lower, upper)), Discrete -> td, Some upper.mean
    | Neither, _ -> td, None
    end
  end

let percentiles td ps = List.fold_map ps ~init:td ~f:percentile

let p_rank td p =
  begin match Float.Map.min_elt td.centroids with
  | None -> td, None
  | Some (_k, v) when p < v.mean -> td, Some 0.0
  | Some _ ->
    begin match Float.Map.max_elt td.centroids with
    | None -> td, None
    | Some (_k, v) when p > v.mean -> td, Some 1.0
    | Some _ ->
      let td = cumulate td true in
      begin match (bounds td p (fun { mean; _ } -> mean)), td.settings.delta with
      | (Both (lower, _)), Discrete
      | (Lower lower), Discrete
      | (Equal lower), Discrete -> td, Some (lower.cumn / td.n)

      | Neither, Discrete
      | (Upper _), Discrete -> td, None

      | (Equal x), Merging _ -> td, Some (x.mean_cumn / td.n)

      | (Both (lower, upper)), Merging _ ->
        let num = lower.mean_cumn + ((p - lower.mean) * (upper.mean_cumn - lower.mean_cumn) / (upper.mean - lower.mean)) in
        td, Some (num / td.n)

      | _, Merging _ -> td, None
      end
    end
  end

let p_ranks td ps = List.fold_map ps ~init:td ~f:p_rank

module Testing = struct
  let centroid_to_yojson data basic =
    let base = [
      "mean", `Float data.mean;
      "n", `Float data.n;
    ]
    in
    `Assoc (
      if basic then base else ("cumn", `Float data.cumn) :: ("mean_cumn", `Float data.mean_cumn) :: base
    )

  let to_yojson td basic =
    let ll = Float.Map.fold_right td.centroids ~init:[] ~f:(fun ~key:_ ~data acc ->
        let base = [
          "mean", `Float data.mean;
          "n", `Float data.n;
        ]
        in
        `Assoc (
          if basic then base else ("cumn", `Float data.cumn) :: ("mean_cumn", `Float data.mean_cumn) :: base
        ) :: acc
      )
    in
    `Assoc ["centroids", `List ll]

  let min td = begin match Float.Map.min_elt td.centroids with
  | None -> `Null
  | Some (_k, v) -> centroid_to_yojson v true
  end

  let max td = begin match Float.Map.max_elt td.centroids with
  | None -> `Null
  | Some (_k, v) -> centroid_to_yojson v true
  end
end
