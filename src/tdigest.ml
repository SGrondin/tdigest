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
} [@@deriving sexp]

type bounds =
| Neither
| Both of (centroid * centroid)
| Equal of centroid
| Lower of centroid
| Upper of centroid

let get_min = function
| { min = (Some _ as x); _ } -> x
| { min = None; centroids; _ } when Float.Map.is_empty centroids -> None
| ({ min = None; _ } as td) ->
  let min = Float.Map.min_elt td.centroids |> Option.map ~f:snd in
  td.min <- min;
  min

let get_max = function
| { max = (Some _ as x); _ } -> x
| { max = None; centroids; _ } when Float.Map.is_empty centroids -> None
| ({ max = None; _ } as td) ->
  let max = Float.Map.max_elt td.centroids |> Option.map ~f:snd in
  td.max <- max;
  max

let create ?(delta = Merging 0.01) ?(k = Automatic 25.) ?(cx = Growth 1.1) () =
  let k = begin match k with
  | Manual -> k
  | Automatic x when Float.is_positive x -> k
  | Automatic 0. -> failwith "TDigest k parameter cannot be zero, set to Tdigest.Manual to disable automatic compression."
  | Automatic x -> failwithf "TDigest k parameter must be positive, but was %f" x ()
  end
  in
  let cx = begin match cx with
  | Always -> cx
  | Growth x when Float.is_positive x -> cx
  | Growth 0. -> failwith "TDigest cx parameter cannot be zero, set to Tdigest.Always to disable caching of cumulative totals."
  | Growth x -> failwithf "TDigest cx parameter must be positive, but was %f" x ()
  end
  in
  {
    settings = {
      delta;
      k;
      cx;
    };
    centroids = Float.Map.empty;
    min = None;
    max = None;
    n = 0.;
    last_cumulate = 0.;
    stats = {
      cumulates_count = 0;
      compress_count = 0;
      auto_compress_count = 0;
    };
  }

let info { centroids; n; stats; _ } = {
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

let use_cache = function
| { n; last_cumulate; settings = { cx = Growth cx; _ }; _ } when cx > n / last_cumulate -> true
| _ -> false

let cumulate td ~exact =
  if (td.n = td.last_cumulate) || (not exact && use_cache td)
  then td
  else begin
    let cumn = ref 0. in
    let centroids = Float.Map.map td.centroids ~f:(fun data ->
        let updated = { data with
          mean_cumn = !cumn + data.n / 2.;
          cumn = !cumn + data.n;
        }
        in
        cumn := updated.cumn;
        updated
      )
    in
    { td with
      centroids;
      min = None;
      max = None;
      n = !cumn;
      last_cumulate = !cumn;
      stats = { td.stats with cumulates_count = succ td.stats.cumulates_count }
    }
  end

let new_centroid td ~mean ~n ~cumn =
  let data = { mean; cumn; n; mean_cumn = n / 2. } in
  let centroids = Float.Map.add_exn td.centroids ~key:data.mean ~data in
  { td with
    centroids;
    min = None;
    max = None;
    n = td.n + n;
  }

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
  { td with
    centroids;
    n = td.n + n;
    min = None;
    max = None;
  }

let internal_digest td ~n ~mean =
  let nearest_is_boundary boundary nearest =
    Option.value_map boundary ~default:false ~f:(fun { mean; _ } -> mean = nearest.mean)
  in
  let td = begin match (find_nearest td mean), td.settings.delta with
  | (Some nearest), _ when nearest.mean = mean ->
    add_weight td nearest ~mean ~n
  | (Some nearest), _ when nearest_is_boundary (get_min td) nearest ->
    new_centroid td ~mean ~n ~cumn:0.0
  | (Some nearest), _ when nearest_is_boundary (get_max td) nearest ->
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
  cumulate td ~exact:false

let shuffled_array td =
  if Float.Map.is_empty td.centroids then [||] else
  let arr = Array.create ~len:(Float.Map.length td.centroids) { mean = 0.; n = 0.; cumn = 0.; mean_cumn = 0. } in
  let _i = Float.Map.fold td.centroids ~init:0 ~f:(fun ~key:_ ~data i ->
      Array.set arr i data;
      succ i
    )
  in
  let _i = Array.fold_right arr ~init:(Array.length arr) ~f:(fun _x i ->
      let random = (Random.float 1.0) * (of_int i) |> to_int in
      let current = pred i in
      Array.swap arr random current;
      current
    )
  in
  arr

let rebuild td ~auto =
  let arr = shuffled_array td in
  let blank = { td with
    centroids = Float.Map.empty;
    min = None;
    max = None;
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
  cumulate td ~exact:true

let digest td ?(n = 1) ~mean =
  let td = internal_digest td ~n:(Int.to_float n) ~mean in
  begin match td.settings with
  | { delta = Merging delta; k = Automatic k; _ } when (Float.Map.length td.centroids |> of_int) > k / delta ->
    rebuild td ~auto:true
  | _ -> td
  end

let compress ?delta td =
  begin match delta with
  | None -> rebuild td ~auto:false
  | Some delta ->
    let settings = td.settings in
    let updated = rebuild { td with settings = { td.settings with delta } } ~auto:false in
    { updated with settings }
  end

let add ?(n = 1) ~data td = digest td ~n ~mean:data

let add_list ?(n = 1) xs td = List.fold xs ~init:td ~f:(fun acc mean -> digest acc ~n ~mean)

let to_string td =
  let buf = Buffer.create Int.(Float.Map.length td.centroids * 16) in
  let add_float f =
    let v = Int64.bits_of_float f in
    let rec loop = function
    | 8 -> ()
    | i ->
      Buffer.add_char buf Int64.(255L land (shift_right v Int.(i * 8)) |> to_int_exn |> Char.of_int_exn);
      loop (succ i)
    in
    loop 0
  in
  Float.Map.iter td.centroids ~f:(fun { mean; n; _ } ->
    add_float mean;
    add_float n
  );
  td, (Buffer.contents buf)

let of_string ?(delta = Merging 0.01) ?(k = Automatic 25.) ?(cx = Growth 1.1) str =
  if Int.(String.length str % 16 <> 0) then raise (Invalid_argument "Invalid string length for Tdigest.of_string");
  let td = create ~delta ~k ~cx () in
  let _i, _mean, _n, centroids = String.fold str ~init:(0, 0L, 0L, Float.Map.empty) ~f:(fun (i, pmean, pn, acc) c ->
      let x = c |> Char.to_int |> Int64.of_int_exn in
      begin match i with
      | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 ->
        let mean = Int64.(pmean lor (shift_left x Int.(i * 8))) in
        (succ i, mean, pn, acc)
      | 8 | 9 | 10 | 11 | 12 | 13 | 14 ->
        let n = Int64.(pn lor (shift_left x Int.((i - 8) * 8))) in
        (succ i, pmean, n, acc)
      | 15 ->
        let mean = Int64.float_of_bits pmean in
        let n = Int64.(pn lor (shift_left x 56) |> float_of_bits) in
        let acc = Float.Map.update acc mean ~f:(function
          | None -> { mean; n; cumn = 0.; mean_cumn = 0. }
          | Some c -> { c with n = c.n + n }
          )
        in
        (0, 0L, 0L, acc)
      | x -> failwithf "Tdigest.of_string: impossible case '%d'. Please report this bug." x ()
      end
    )
  in
  rebuild { td with centroids } ~auto:true

let bounds td needle lens =
  let search kind =
    Float.Map.binary_search td.centroids kind needle
      ~compare:(fun ~key:_ ~data x -> compare (lens data) x)
  in
  begin match search `Last_less_than_or_equal_to with
  | Some (_k, v) when (lens v) = needle -> Equal v
  | Some (_k1, v1) ->
    begin match search `First_strictly_greater_than with
    | Some (_k2, v2) -> Both (v1, v2)
    | None -> Lower v1
    end
  | None ->
    begin match get_min td with
    | Some v -> Upper v
    | None -> Neither
    end
  end

let percentile td p =
  if Float.Map.is_empty td.centroids then td, None else begin
    let td = cumulate td ~exact:true in
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
  begin match get_min td with
  | None -> td, None
  | Some v when p < v.mean -> td, Some 0.0
  | Some _ ->
    begin match get_max td with
    | None -> td, None
    | Some v when p > v.mean -> td, Some 1.0
    | Some _ ->
      let td = cumulate td ~exact:true in
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

  let to_yojson td =
    let ll = Float.Map.fold_right td.centroids ~init:[] ~f:(fun ~key:_ ~data acc ->
        `Assoc [
          "mean", `Float data.mean;
          "n", `Float data.n;
        ] :: acc
      )
    in
    `Assoc ["centroids", `List ll]

  let min td = Option.map (get_min td) ~f:(fun { mean; n; _ } -> mean, n)

  let max td = Option.map (get_max td) ~f:(fun { mean; n; _ } -> mean, n)

end
