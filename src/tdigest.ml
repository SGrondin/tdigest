open! Base
module Map = Cmap
open Option.Monad_infix

type delta =
  | Merging of float
  | Discrete
[@@deriving sexp]

type k =
  | Manual
  | Automatic of float
[@@deriving sexp]

type cx =
  | Always
  | Growth of float
[@@deriving sexp]

let default_delta = Merging 0.01

let default_k = Automatic 25.0

let default_cx = Growth 1.1

type settings = {
  delta: delta;
  k: k;
  cx: cx;
  k_delta: float option;
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
[@@deriving sexp]

let empty_stats = { cumulates_count = 0; compress_count = 0; auto_compress_count = 0 }

type info = {
  count: int;
  size: int;
  cumulates_count: int;
  compress_count: int;
  auto_compress_count: int;
}
[@@deriving sexp]

module type M = sig
  type 'a t

  val empty : 'a t

  module Map : sig
    val is_empty : 'a t -> bool

    val length : 'a t -> int

    val min_elt : 'a t -> (float * 'a) option

    val max_elt : 'a t -> (float * 'a) option

    val iter : 'a t -> f:('a -> unit) -> unit

    val map : 'a t -> f:('a -> 'b) -> 'b t

    val fold : 'a t -> init:'b -> f:(key:float -> data:'a -> 'b -> 'b) -> 'b

    val fold_right : 'a t -> init:'b -> f:(key:float -> data:'a -> 'b -> 'b) -> 'b

    val binary_search :
      'a t ->
      compare:(key:float -> data:'a -> float -> int) ->
      Binary_searchable.Which_target_by_key.t ->
      float ->
      (float * 'a) option

    val add_exn : 'a t -> key:float -> data:'a -> 'a t

    val remove : 'a t -> float -> 'a t
  end
end

module Make (M : M) = struct
  open Float

  type t = {
    settings: settings;
    centroids: centroid M.t;
    mutable min: centroid option;
    mutable max: centroid option;
    n: float;
    last_cumulate: float;
    stats: stats;
  }

  let get_min = function
  | { min = Some _ as x; _ } -> x
  | { min = None; n = 0.0; _ } -> None
  | { min = None; _ } as td ->
    let min = M.Map.min_elt td.centroids >>| snd in
    td.min <- min;
    min

  let get_max = function
  | { max = Some _ as x; _ } -> x
  | { max = None; n = 0.0; _ } -> None
  | { max = None; _ } as td ->
    let max = M.Map.max_elt td.centroids >>| snd in
    td.max <- max;
    max

  let get_k_delta = function
  | Automatic k, Merging delta -> Some (k / delta)
  | _ -> None

  let create ?(delta = default_delta) ?(k = default_k) ?(cx = default_cx) () =
    let k =
      match k with
      | Manual -> k
      | Automatic x when is_positive x -> k
      | Automatic 0.0 ->
        invalid_arg
          "TDigest.create: k parameter cannot be zero, set to Tdigest.Manual to disable automatic \
           compression."
      | Automatic x -> Printf.invalid_argf "TDigest k parameter must be positive, but was %f" x ()
    in
    let cx =
      match cx with
      | Always -> cx
      | Growth x when is_positive x -> cx
      | Growth 0.0 ->
        invalid_arg
          "TDigest.create: cx parameter cannot be zero, set to Tdigest.Always to disable caching of \
           cumulative totals."
      | Growth x -> Printf.invalid_argf "TDigest.create: cx parameter must be positive, but was %f" x ()
    in
    {
      settings = { delta; k; cx; k_delta = get_k_delta (k, delta) };
      centroids = M.empty;
      min = None;
      max = None;
      n = 0.0;
      last_cumulate = 0.0;
      stats = empty_stats;
    }

  let is_empty { centroids; _ } = M.Map.is_empty centroids

  let info { centroids; n; stats; _ } =
    {
      count = to_int n;
      size = M.Map.length centroids;
      cumulates_count = stats.cumulates_count;
      compress_count = stats.compress_count;
      auto_compress_count = stats.auto_compress_count;
    }

  let find_nearest td mean =
    let gt = ref None in
    let lte =
      M.Map.binary_search td.centroids `Last_less_than_or_equal_to mean
        ~compare:(fun ~key ~data against ->
        let x = compare key against in
        if Int.is_positive x then gt := Some (key, data);
        x )
    in
    match lte with
    | Some (k, v) when mean = k -> Some v
    | Some (k1, v1) -> (
      match !gt with
      | None -> None
      | Some (k2, _v2) when mean - k1 < k2 - mean -> Some v1
      | Some (_k2, v2) -> Some v2 )
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
        M.Map.map td.centroids ~f:(fun data ->
          let updated = { data with mean_cumn = !cumn + (data.n / 2.); cumn = !cumn + data.n } in
          cumn := updated.cumn;
          updated )
      in
      {
        td with
        centroids;
        min = None;
        max = None;
        n = !cumn;
        last_cumulate = !cumn;
        stats = { td.stats with cumulates_count = Int.succ td.stats.cumulates_count };
      } )

  let new_bounds ({ mean; _ } as added) = function
  | { n = 0.0; min = None; max = None; _ } -> Some added, Some added
  | { min = Some existing; max; _ } when mean < existing.mean -> Some added, max
  | { min; max = Some existing; _ } when mean > existing.mean -> min, Some added
  | { min; max; _ } -> min, max

  let new_centroid td ~mean ~n ~cumn =
    let data = { mean; cumn; n; mean_cumn = n / 2. } in
    let centroids = M.Map.add_exn td.centroids ~key:mean ~data in
    let min, max = new_bounds data td in
    { td with centroids; min; max; n = td.n + n }

  let add_weight td nearest ~mean ~n =
    let updated =
      {
        mean =
          ( if nearest.mean = mean
            then nearest.mean
            else nearest.mean + (n * (mean - nearest.mean) / (nearest.n + n)) );
        cumn = nearest.cumn + n;
        mean_cumn = nearest.mean_cumn + (n / 2.);
        n = nearest.n + n;
      }
    in
    let centroids =
      M.Map.remove td.centroids nearest.mean |> M.Map.add_exn ~key:updated.mean ~data:updated
    in
    { td with centroids; n = td.n + n; min = None; max = None }

  let internal_digest td ~n ~mean =
    let nearest_is_boundary boundary nearest =
      Option.value_map boundary ~default:false ~f:(fun { mean; _ } -> mean = nearest.mean)
    in
    let td =
      match find_nearest td mean, td.settings.delta with
      | Some nearest, _ when nearest.mean = mean -> add_weight td nearest ~mean ~n
      | Some nearest, _ when nearest_is_boundary (get_min td) nearest ->
        new_centroid td ~mean ~n ~cumn:0.0
      | Some nearest, _ when nearest_is_boundary (get_max td) nearest ->
        new_centroid td ~mean ~n ~cumn:td.n
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

  let weights_of_td = function
  (* n is out of sync, must check centroids *)
  | { centroids; _ } when M.Map.is_empty centroids -> [||]
  | { centroids; _ } ->
    let arr = Array.create ~len:(M.Map.length centroids) empty_centroid in
    let _i =
      M.Map.fold centroids ~init:0 ~f:(fun ~key:_ ~data i ->
        arr.(i) <- data;
        Int.succ i )
    in
    arr

  let weights_of_table table =
    let arr = Array.create ~len:(Hashtbl.length table) empty_centroid in
    let _i =
      Hashtbl.fold table ~init:0 ~f:(fun ~key:mean ~data:n i ->
        arr.(i) <- { empty_centroid with mean; n };
        Int.succ i )
    in
    arr

  let rebuild ~auto settings (stats : stats) arr =
    Array.permute arr;
    let blank =
      {
        settings;
        centroids = M.empty;
        min = None;
        max = None;
        n = 0.0;
        last_cumulate = 0.0;
        stats =
          {
            stats with
            compress_count = Int.succ stats.compress_count;
            auto_compress_count = (if auto then Int.succ else Fn.id) stats.auto_compress_count;
          };
      }
    in
    let td = Array.fold arr ~init:blank ~f:(fun acc { mean; n; _ } -> internal_digest acc ~n ~mean) in
    cumulate td ~exact:true

  let digest ?(n = 1) td ~mean =
    let td = internal_digest td ~n:(of_int n) ~mean in
    match td.settings with
    | { k_delta = Some kd; _ } when M.Map.length td.centroids |> of_int > kd ->
      rebuild ~auto:true td.settings td.stats (weights_of_td td)
    | _ -> td

  let add ?(n = 1) ~data td =
    if Int.(n <= 0) then invalid_arg "Tdigest.add: n <= 0";
    digest td ~n ~mean:data

  let add_list ?(n = 1) xs td =
    if Int.(n <= 0) then invalid_arg "Tdigest.add_list: n <= 0";
    List.fold xs ~init:td ~f:(fun acc mean -> digest acc ~n ~mean)

  let compress ?delta td =
    match delta with
    | None -> rebuild ~auto:false td.settings td.stats (weights_of_td td)
    | Some delta ->
      let settings = td.settings in
      let updated = rebuild ~auto:false { td.settings with delta } td.stats (weights_of_td td) in
      { updated with settings }

  let to_string td =
    let buf = Bytes.create (M.Map.length td.centroids |> Int.( * ) 16) in
    let add_float pos ~data:f =
      let v = Int64.bits_of_float f in
      let rec loop pos = function
        | 8 -> pos
        | i ->
          Bytes.set buf pos Int64.(255L land shift_right v Int.(i * 8) |> to_int_exn |> Char.of_int_exn);
          (loop [@tailcall]) (Int.succ pos) (Int.succ i)
      in
      loop pos 0
    in
    let _pos =
      M.Map.fold td.centroids ~init:0 ~f:(fun ~key:_ ~data:{ mean; n; _ } pos ->
        add_float pos ~data:mean |> add_float ~data:n )
    in
    td, Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf

  let parse_float str pos =
    let open Int64 in
    let next off = String.get str Int.(pos + off) |> Char.to_int |> of_int_exn in
    (String.get str pos |> Char.to_int |> of_int_exn)
    lor shift_left (next 1) 8
    lor shift_left (next 2) 16
    lor shift_left (next 3) 24
    lor shift_left (next 4) 32
    lor shift_left (next 5) 40
    lor shift_left (next 6) 48
    lor shift_left (next 7) 56
    |> float_of_bits

  let of_string ?(delta = default_delta) ?(k = default_k) ?(cx = default_cx) str =
    if Int.(String.length str % 16 <> 0) then invalid_arg "Tdigest.of_string: invalid string length";
    let settings = { delta; k; cx; k_delta = get_k_delta (k, delta) } in
    let table = Hashtbl.create (module Float) in
    let rec loop = function
      | pos when Int.(pos = String.length str) -> ()
      | pos ->
        let mean = parse_float str pos in
        let n = parse_float str Int.(pos + 8) in
        Hashtbl.update table mean ~f:(Option.value_map ~default:n ~f:(( + ) n));
        (loop [@tailcall]) Int.(pos + 16)
    in
    loop 0;
    weights_of_table table |> rebuild ~auto:true settings empty_stats

  module Export = struct
    type td_t = t

    type settings = {
      delta: delta;
      k: k;
      cx: cx;
    }
    [@@deriving sexp]

    type t = {
      settings: settings;
      state: string;
      stats: stats;
    }
    [@@deriving sexp]

    let create ({ settings = { delta; k; cx; _ }; stats; _ } as td : td_t) =
      { settings = { delta; k; cx }; state = to_string td |> snd; stats }
  end

  let sexp_of_t td = Export.create td |> [%sexp_of: Export.t]

  let t_of_sexp sexp =
    let Export.{ settings = { delta; k; cx }; state; stats } = [%of_sexp: Export.t] sexp in
    { (of_string ~delta ~k ~cx state) with stats }

  let merge ?(delta = default_delta) ?(k = default_k) ?(cx = default_cx) tds =
    let settings = { delta; k; cx; k_delta = get_k_delta (k, delta) } in
    let table = Hashtbl.create (module Float) in
    List.iter tds ~f:(fun { centroids; _ } ->
      M.Map.iter centroids ~f:(fun { mean; n; _ } ->
        Hashtbl.update table mean ~f:(Option.value_map ~default:n ~f:(( + ) n)) ) );
    weights_of_table table |> rebuild ~auto:true settings empty_stats

  type bounds =
    | Neither
    | Both of centroid * centroid
    | Equal of centroid
    | Lower of centroid
    | Upper of centroid

  let bounds td needle lens =
    let gt = ref None in
    let lte =
      M.Map.binary_search td.centroids `Last_less_than_or_equal_to needle
        ~compare:(fun ~key ~data against ->
        let x = compare (lens data) against in
        if Int.is_positive x then gt := Some (key, data);
        x )
    in
    match lte with
    | Some (_k, v) when lens v = needle -> Equal v
    | Some (_k1, v1) -> (
      match !gt with
      | Some (_k2, v2) -> Both (v1, v2)
      | None -> Lower v1 )
    | None -> (
      match get_min td with
      | Some v -> Upper v
      | None -> Neither )

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
      | Neither, _ -> td, None )

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
        | _, Merging _ -> td, None ) )

  let p_ranks td ps = List.fold_map ps ~init:td ~f:p_rank

  module Private = struct
    let centroids td =
      M.Map.fold_right td.centroids ~init:[] ~f:(fun ~key:_ ~data:{ mean; n; _ } acc -> (mean, n) :: acc)

    let min td = get_min td >>| fun { mean; n; _ } -> mean, n

    let max td = get_max td >>| fun { mean; n; _ } -> mean, n
  end
end

module M = Make (struct
  type 'a t = (float, 'a, Float.comparator_witness) Map.t

  let empty = Map.empty (module Float)

  module Map = Map
end)

module Marshallable = Make (struct
  type 'a t = (float, 'a, Float.comparator_witness) Map.Tree.t

  module Map = Map.Make_tree (Float)

  let empty = Map.empty
end)

include M
