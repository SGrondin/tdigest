open! Core_kernel

type delta =
| Compress of float
| Discrete

type t

val create: ?delta:delta -> ?k:int -> ?cx:float -> unit -> t

val add: ?n:int -> mean:float -> t -> t

val add_list: ?n:int -> float list -> t -> t

val p_rank: t -> float -> t * float option
val p_ranks: t -> float list -> t * float option list

val percentile: t -> float -> t * float option
val percentiles: t -> float list -> t * float option list

val size: t -> int

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

  val compress_with_delta: t -> delta -> t

  val compress: t -> t

  val min:
    t ->
    [> `Assoc of (string * [> `Float of float ]) list | `Null ]

  val max:
    t ->
    [> `Assoc of (string * [> `Float of float ]) list | `Null ]
end
