type 'a t

val empty : 'a t

val is_empty : 'a t -> bool

val add : float -> 'a -> 'a t -> 'a t

val min_binding_opt : 'a t -> (float * 'a) option

val max_binding_opt : 'a t -> (float * 'a) option

val remove : float -> 'a t -> 'a t

val cardinal : 'a t -> int

val iter : (float -> 'a -> unit) -> 'a t -> unit

val map : ('a -> 'b) -> 'a t -> 'b t

val fold : (float -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

val binary_search :
  'a t ->
  compare:(key:float -> data:'a -> 'b -> int) ->
  [< `First_equal_to
  | `First_greater_than_or_equal_to
  | `First_strictly_greater_than
  | `Last_equal_to
  | `Last_less_than_or_equal_to
  | `Last_strictly_less_than
  ] ->
  'b ->
  (float * 'a) option

val to_rev_seq : 'a t -> (float * 'a) Seq.t
