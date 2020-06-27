open! Core_kernel

type delta =
| Merging of float
| Discrete

type k =
| Manual
| Automatic of float

type cx =
| Always
| Growth of float

type t

type info = {
  count: int;
  size: int;
  cumulates_count: int;
  compress_count:int;
  auto_compress_count: int;
}

val create: ?delta:delta -> ?compression:k -> ?refresh:cx -> unit -> t

val info: t -> info

val add: ?n:int -> mean:float -> t -> t

val add_list: ?n:int -> float list -> t -> t

val p_rank: t -> float -> t * float option
val p_ranks: t -> float list -> t * float option list

val percentile: t -> float -> t * float option
val percentiles: t -> float list -> t * float option list

val compress: ?delta:delta -> t -> t

module Testing : sig
  val to_yojson:
    t ->
    bool ->
    [> `Assoc of
        (string *
            [> `List of
                [> `Assoc of (string * [> `Float of float ]) list ]
                  list ])
          list ]

  val min:
    t ->
    [> `Assoc of (string * [> `Float of float ]) list | `Null ]

  val max:
    t ->
    [> `Assoc of (string * [> `Float of float ]) list | `Null ]
end
