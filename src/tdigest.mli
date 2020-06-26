open! Core_kernel

type delta =
| Compress of float
| Discrete

type t

val create: ?delta:delta -> ?k:int -> ?cx:float -> unit -> t

val add: t -> ?n:int -> float -> unit

val add_list: t -> ?n:int -> float list -> unit

val p_rank: t -> float -> float option

val percentile: t -> float -> float option

val summary: t -> string

module Testing : sig
  val to_yojson:
    t ->
    bool ->
    [> `Assoc of
        (string *
            [> `List of
                [> `Assoc of (string * [> `Float of float | `Int of int ]) list ]
                  list ])
          list ]

  val compress_with_delta: t -> delta -> unit

  val compress: t -> unit

  val size: t -> int

  val min:
    t ->
    [> `Assoc of (string * [> `Float of float | `Int of int ]) list | `Null ]

  val max:
    t ->
    [> `Assoc of (string * [> `Float of float | `Int of int ]) list | `Null ]
end
