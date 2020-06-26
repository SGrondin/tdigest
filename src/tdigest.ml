open! Core_kernel
open Float

type delta =
| Compress of float
| Discrete

type settings = {
  delta: delta;
  k: float;
  cx: float;
}

type centroid = {
  mean: float;
  cumn: float;
  mean_cumn: float;
  n: float;
}

type t = {
  settings: settings;
  centroids: centroid Float.Map.t;
  n: float;
  last_cumulate: float;
}

type bounds =
| Neither
| Both of (centroid * centroid)
| Equal of centroid
| Lower of centroid
| Upper of centroid


let create ?(delta = Compress 0.01) ?(k = 25) ?(cx = 1.1) () =
  (* TODO: validate inputs *)
  {
    settings = { delta; k = Int.to_float k; cx };
    centroids = Float.Map.empty;
    n = 0.;
    last_cumulate = 0.;
  }

let size td = Float.Map.length td.centroids

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
  begin match Float.Map.add td.centroids ~key:data.mean ~data with
  | `Duplicate -> failwith "Insert ignored!"
  | `Ok centroids ->
    { td with centroids; n = td.n + data.n }
  end

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
  Float.Map.remove td.centroids nearest.mean
  |> Float.Map.add ~key:updated.mean ~data:updated
  |> function
  | `Duplicate -> failwith "Update ignored!"
  | `Ok centroids ->
    { td with centroids; n = td.n + n }

let cumulate td exact =
  if (td.n = td.last_cumulate) || (not exact && td.settings.cx > 0.0 && td.settings.cx > td.n / td.last_cumulate)
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
    { td with centroids; n = cumn; last_cumulate = cumn }
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
  | (Some nearest), Compress delta ->
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
    let arr = Array.create ~len:(size td) v in
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

let compress td =
  let arr = shuffled_array td in
  let blank = { td with
    centroids = Float.Map.empty;
    n = 0.;
    last_cumulate = 0.;
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
  | { delta = Compress delta; k; _ } when is_positive k && (size td |> of_int) > k / delta ->
    compress td
  | _ -> td
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
    | (Both (lower, upper)), Compress _ ->
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

      | (Equal x), Compress _ -> td, Some (x.mean_cumn / td.n)

      | (Both (lower, upper)), Compress _ ->
        let num = lower.mean_cumn + ((p - lower.mean) * (upper.mean_cumn - lower.mean_cumn) / (upper.mean - lower.mean)) in
        td, Some (num / td.n)

      | _, Compress _ -> td, None
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

  let compress_with_delta td delta =
    let settings = td.settings in
    let updated = compress { td with settings = { td.settings with delta } } in
    { updated with settings }

  let compress = compress

  let min td = begin match Float.Map.min_elt td.centroids with
  | None -> `Null
  | Some (_k, v) -> centroid_to_yojson v true
  end

  let max td = begin match Float.Map.max_elt td.centroids with
  | None -> `Null
  | Some (_k, v) -> centroid_to_yojson v true
  end
end
