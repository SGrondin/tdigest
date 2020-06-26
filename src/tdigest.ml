open! Core_kernel

type delta =
| Compress of float
| Discrete

type settings = {
  delta: delta;
  k: int;
  cx: float;
}

type centroid = {
  mutable mean: float;
  mutable cumn: float;
  mutable mean_cumn: float;
  mutable n: int;
}

type t = {
  mutable settings: settings;
  mutable centroids: centroid Float.Map.t;
  mutable n: int;
  mutable last_cumulate: int;
}

let is_discrete = function
| { settings = { delta = Discrete; _ }; _ } -> true
| _ -> false

let create ?(delta = Compress 0.01) ?(k = 25) ?(cx = 1.1) () =
  (* TODO: validate inputs *)
  {
    settings = { delta; k; cx };
    centroids = Float.Map.empty;
    n = 0;
    last_cumulate = 0;
  }

let size td = Float.Map.length td.centroids

let find_nearest td mean =
  begin match Float.Map.closest_key td.centroids `Less_or_equal_to mean with
  | Some (k, v) when Float.(mean = k) -> Some v
  | Some (k1, v1) ->
    begin match Float.Map.closest_key td.centroids `Greater_than mean with
    | None -> None
    | Some (k2, _v2) when Float.((mean - k1) < (k2 - mean)) -> Some v1
    | Some (_k2, v2) -> Some v2
    end
  | None -> None
  end

let insert td c =
  begin match Float.Map.add td.centroids ~key:c.mean ~data:c with
  | `Ok centroids ->
    td.centroids <- centroids;
    td.n <- td.n + c.n
  | `Duplicate ->
    failwith "Insert ignored!"
  end

let new_centroid td ~mean ~n ~cumn =
  let data = { mean; cumn; n; mean_cumn = n // 2 } in
  insert td data

let add_weight td nearest ~mean ~n =
  let open Float in
  if nearest.mean <> mean
  then nearest.mean <- nearest.mean + ((of_int n) * (mean - nearest.mean) / Int.(nearest.n + n |> to_float));
  nearest.cumn <- nearest.cumn + (of_int n);
  nearest.mean_cumn <- nearest.mean_cumn + (nearest.n // 2);
  nearest.n <- Int.(nearest.n + n);
  td.n <- Int.(td.n + n)

let cumulate td exact =
  let open Float in
  if Int.(td.n = td.last_cumulate) ||
     (not exact && td.settings.cx > 0.0 && td.settings.cx > td.n // td.last_cumulate)
  then () else begin
    let cumn = Float.Map.fold td.centroids ~init:0 ~f:(fun ~key:_ ~data cumn ->
        data.mean_cumn <- (of_int cumn) + data.n // 2;
        let acc = Int.(cumn + data.n) in
        data.cumn <- of_int acc;
        acc
      )
    in
    td.n <- cumn;
    td.last_cumulate <- cumn
  end

let internal_digest td ~n ~mean =
  let nearest_is_fn fn nearest =
    Option.value_map ~default:false (fn td.centroids) ~f:Float.(fun (k, _v) -> k = nearest.mean)
  in
  begin match (find_nearest td mean), td.settings.delta with
  | (Some nearest), _ when Float.(nearest.mean = mean) ->
    add_weight td nearest ~mean ~n
  | (Some nearest), _ when nearest_is_fn Float.Map.min_elt nearest ->
    new_centroid td ~mean ~n ~cumn:0.0
  | (Some nearest), _ when nearest_is_fn Float.Map.max_elt nearest ->
    new_centroid td ~mean ~n ~cumn:(Int.to_float td.n)
  | (Some nearest), Discrete ->
    new_centroid td ~mean ~n ~cumn:nearest.cumn
  | (Some nearest), Compress delta ->
    let p = Float.(nearest.mean_cumn / (of_int td.n)) in
    let max_n = Float.(round_down (4.0 * (of_int td.n) * delta * p * (1.0 - p))) in
    if Float.((max_n - (of_int nearest.n)) >= (of_int n))
    then add_weight td nearest ~mean ~n
    else new_centroid td ~mean ~n ~cumn:nearest.cumn
  | None, _ ->
    new_centroid td ~mean ~n ~cumn:0.0
  end;
  cumulate td false

let shuffled_array td =
  let arr = Array.create ~len:(size td) (Float.Map.min_elt_exn td.centroids |> snd) in
  let _i = Float.Map.fold td.centroids ~init:0 ~f:(fun ~key:_ ~data i ->
      Array.set arr i data;
      i + 1
    )
  in
  let _i = Array.fold_right arr ~init:(Array.length arr) ~f:(fun _x i ->
      let random = Float.((Random.float 1.0) * (of_int i) |> to_int) in
      let current = i - 1 in
      Array.swap arr random current;
      current
    )
  in
  arr

let compress td =
  let arr = shuffled_array td in
  td.centroids <- Float.Map.empty;
  td.n <- 0;
  td.last_cumulate <- 0;
  Array.iter arr ~f:(fun { mean; n; _ } ->
    internal_digest td ~n ~mean
  );
  cumulate td true

let digest td ?(n = 1) ~mean =
  internal_digest td ~n ~mean;
  begin match td.settings with
  | { delta = Compress delta; k; _ } when k > 0 && Float.((size td |> of_int) > (of_int k) / delta) ->
    compress td
  | _ -> ()
  end

let add td ?(n = 1) mean = digest td ~n ~mean

let add_list td ?(n = 1) xs = List.iter xs ~f:(fun mean -> digest td ~n ~mean)

let bounds td needle lens =
  let search kind =
    Float.Map.binary_search td.centroids kind needle
      ~compare:(fun ~key:_ ~data x -> Float.compare (lens data) x)
  in
  begin match search `Last_less_than_or_equal_to with
  | Some (_k, v) when Float.((lens v) = needle) ->
    (Some v), (Some v)
  | Some (_k1, v1) ->
    begin match search `First_strictly_greater_than with
    | Some (_k2, v2) ->
      (Some v1), (Some v2)
    | None ->
      (Some v1, None)
    end
  | None -> None, (Float.Map.min_elt td.centroids |> Option.map ~f:snd)
  end

let percentile td p =
  if Float.Map.is_empty td.centroids then None else begin
    cumulate td true;
    let h = Float.((of_int td.n) * p) in
    begin match (bounds td h (fun { mean_cumn; _ } -> mean_cumn)), td.settings.delta with
    | ((Some x), None), _
    | (None, (Some x)), _ -> Some x.mean
    | ((Some { mean = lower; _ }), (Some { mean = upper; _ })), _ when Float.(lower = upper) -> Some lower
    | ((Some lower), (Some upper)), Compress _ ->
      Some Float.(lower.mean + (h - lower.mean_cumn) * (upper.mean - lower.mean) / (upper.mean_cumn - lower.mean_cumn))
    | ((Some lower), (Some _)), Discrete when Float.(h <= lower.cumn) -> Some lower.mean
    | ((Some _), (Some upper)), Discrete -> Some upper.mean
    | (None, None), _ -> None
    end
  end

let summary td =
  let print_pct x = percentile td x |> Option.value_map ~f:Float.to_string ~default:"---" in
  String.concat ~sep:"\n" [
    sprintf "%s %d samples using %d centroids"
      (if is_discrete td then "approx" else "exact") td.n (size td);
    sprintf "min = %s" (print_pct 0.0);
    sprintf "Q1  = %s" (print_pct 0.25);
    sprintf "Q2  = %s" (print_pct 0.5);
    sprintf "Q3  = %s" (print_pct 0.75);
    sprintf "max = %s" (print_pct 1.0);
  ]

let p_rank td p =
  let open Float in
  begin match Float.Map.min_elt td.centroids with
  | None -> None
  | Some (_k, v) when p < v.mean -> Some 0.0
  | Some _ ->
    begin match Float.Map.max_elt td.centroids with
    | None -> None
    | Some (_k, v) when p > v.mean -> Some 1.0
    | Some _ ->
      cumulate td true;
      begin match (bounds td p (fun { mean; _ } -> mean)), td.settings.delta with
      | ((Some lower), _), Discrete -> Some (lower.cumn / (of_int td.n))
      | (None, _), Discrete -> None
      | ((Some { mean_cumn = lower; _ }), (Some { mean_cumn = upper; _ })), Compress _ when Float.(lower = upper) ->
        Some (lower / (of_int td.n))
      | ((Some lower), (Some upper)), Compress _ ->
        let num = lower.mean_cumn + ((p - lower.mean) * (upper.mean_cumn - lower.mean_cumn) / (upper.mean - lower.mean)) in
        Some (num / (of_int td.n))
      | (_, _), Compress _ -> None
      end
    end
  end

module Testing = struct
  let centroid_to_yojson data basic =
    let base = [
      "mean", `Float data.mean;
      "n", `Int data.n;
    ]
    in
    `Assoc (
      if basic then base else ("cumn", `Float data.cumn) :: ("mean_cumn", `Float data.mean_cumn) :: base
    )

  let to_yojson td basic =
    let ll = Float.Map.fold_right td.centroids ~init:[] ~f:(fun ~key:_ ~data acc ->
        let base = [
          "mean", `Float data.mean;
          "n", `Int data.n;
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
    td.settings <- { td.settings with delta };
    compress td;
    td.settings <- settings

  let compress = compress

  let size = size

  let min td = begin match Float.Map.min_elt td.centroids with
  | None -> `Null
  | Some (_k, v) -> centroid_to_yojson v true
  end

  let max td = begin match Float.Map.max_elt td.centroids with
  | None -> `Null
  | Some (_k, v) -> centroid_to_yojson v true
  end
end
