open! Core

(**
   [delta] is the compression factor, the max fraction of mass that can be owned by one centroid (bigger, up to 1.0, means more compression).
   [~delta:Discrete] switches off TDigest behavior and treats the distribution as discrete, with no merging and exact values reported.
*)
type delta =
  | Merging  of float
  | Discrete

(**
   [k] is a size threshold that triggers recompression as the TDigest grows during input.
   [~k:Manual] disables automatic recompression.
*)
type k =
  | Manual
  | Automatic of float

(**
   [cx] (default: [1.1]) specifies how often to update cached cumulative totals used for quantile estimation during ingest.
   This is a tradeoff between performance and accuracy.
   [~cx:Always] will recompute cumulatives on every new datapoint, but the performance drops by 15-25x or even more depending on the size of the dataset.
*)
type cx =
  | Always
  | Growth of float

type t

(**
   [count]: sum of all [n]

   [size]: size of the internal B-Tree. Calling [Tdigest.compress] will usually reduce this size.

   [cumulates_count]: number of cumulate operations over the life of this Tdigest instance.

   [compress_count]: number of compression operations over the life of this Tdigest instance.

   [auto_cumulates_count]: number of compression operations over the life of this Tdigest instance that were not triggered by a manual call to [Tdigest.compress].
*)
type info = {
  count: int;
  size: int;
  cumulates_count: int;
  compress_count: int;
  auto_compress_count: int;
}

(**
   [Tdigest.create ?delta ?k ?cx ()]

   Allocate an empty Tdigest instance.

   [delta] (default: [0.01]) is the compression factor, the max fraction of mass that can be owned by one centroid (bigger, up to 1.0, means more compression).
   [~delta:Discrete] switches off TDigest behavior and treats the distribution as discrete, with no merging and exact values reported.

   [k] (default: [25]) is a size threshold that triggers recompression as the TDigest grows during input.
   [~k:Manual] disables automatic recompression.

   [cx] (default: [1.1]) specifies how often to update cached cumulative totals used for quantile estimation during ingest.
   This is a tradeoff between performance and accuracy.
   [~cx:Always] will recompute cumulatives on every new datapoint, but the performance drops by 15-25x or even more depending on the size of the dataset.
*)
val create : ?delta:delta -> ?k:k -> ?cx:cx -> unit -> t

(**
   [Tdigest.info td] returns a record with these fields:

   [count]: sum of all [n]

   [size]: size of the internal B-Tree. Calling [Tdigest.compress] will usually reduce this size.

   [cumulates_count]: number of cumulate operations over the life of this Tdigest instance.

   [compress_count]: number of compression operations over the life of this Tdigest instance.

   [auto_cumulates_count]: number of compression operations over the life of this Tdigest instance that were not triggered by a manual call to [Tdigest.compress].
*)
val info : t -> info

(**
   [Tdigest.add ?n ~data td]

   Incorporate a value ([data]) having count [n] (default: [1]) into a new Tdigest.
*)
val add : ?n:int -> data:float -> t -> t

(**
   [Tdigest.add_list ?n ll td]

   Incorporate a list of values each having count [n] (default: [1]) into a new Tdigest.
*)
val add_list : ?n:int -> float list -> t -> t

(**
   [Tdigest.merge ?delta ?k ?cx tdigests]

   Efficiently combine multiple Tdigests into a new one.
*)
val merge : ?delta:delta -> ?k:k -> ?cx:cx -> t list -> t

(**
   [Tdigest.p_rank td q]
   For a value [q] estimate the percentage ([0..1]) of values [<= q].

   Returns a new Tdigest to reuse intermediate computations.
*)
val p_rank : t -> float -> t * float option

(**
   Same as [Tdigest.p_rank] but for a list of values.

   Returns a new Tdigest to reuse intermediate computations.
*)
val p_ranks : t -> float list -> t * float option list

(**
   [Tdigest.percentile td p]

   For a percentage [p] ([0..1]) estimate the smallest value [q] at which at least [p] percent of the values [<= q].

   For discrete distributions, this selects q using the Nearest Rank Method
   [https://en.wikipedia.org/wiki/Percentile#The_Nearest_Rank_method]

   For continuous distributions, interpolates data values between count-weighted bracketing means.

   Returns a new Tdigest to reuse intermediate computations.
*)
val percentile : t -> float -> t * float option

(**
   Same as [Tdigest.percentile] but for a list of values.

   Returns a new Tdigest to reuse intermediate computations.
*)
val percentiles : t -> float list -> t * float option list

(**
   [Tdigest.compress ?delta td]

   Manual recompression. Not guaranteed to reduce size further if too few values have been added since the last compression.

   [delta] (default: initial value passed to [Tdigest.create]) The compression level to use for this operation only. This does not alter the [delta] used by the Tdigest going forward.
*)
val compress : ?delta:delta -> t -> t

(**
   [Tdigest.to_string td]

   Serialize the internal state into a binary string that can be stored or concatenated with other such binary strings.

   Use [Tdigest.of_string] to create a new Tdigest instance from it.

   Returns a new Tdigest to reuse intermediate computations.
*)
val to_string : t -> t * string

(**
   [Tdigest.of_string ?delta ?k ?cx str]

   See [Tdigest.create] for the meaning of the optional parameters.

   Allocate a new Tdigest from a string or concatenation of strings originally created by [Tdigest.to_string].
*)
val of_string : ?delta:delta -> ?k:k -> ?cx:cx -> string -> t

(** For internal use *)
module Private : sig
  (** For internal use *)
  val to_yojson :
    t ->
    [> `Assoc of (string * [> `List of [> `Assoc of (string * [> `Float of float ]) list ] list ]) list ]

  (** For internal use *)
  val min : t -> (float * float) option

  (** For internal use *)
  val max : t -> (float * float) option
end
