(* Code adapted from https://github.com/janestreet/core/blob/e70fdcdaa308dffb1b5bb8bb38acf46c73b40161/core/src/map.ml and map_intf.ml

   The MIT License

   Copyright (c) 2008--2023 Jane Street Group, LLC opensource-contacts@janestreet.com

   Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)
open! Base
module Tree0 = Map.Using_comparator.Tree

module type Creators_generic = sig
  include Map.Creators_generic

  val of_hashtbl_exn : ('k, 'cmp, ('k key, 'v) Hashtbl.t -> ('k, 'v, 'cmp) t) create_options

  (** Never requires a comparator because it can get one from the input [Set.t]. *)
  val of_key_set : ('k key, 'cmp cmp) Set.t -> f:(('k key -> 'v)[@local]) -> ('k, 'v, 'cmp) t
end

type ('k, 'cmp) comparator = (module Comparator.S with type t = 'k and type comparator_witness = 'cmp)

let to_comparator (type k cmp) ((module M) : (k, cmp) Comparator.Module.t) = M.comparator

let of_comparator (type k cmp) comparator : (k, cmp) Comparator.Module.t =
  ( module struct
    type t = k

    type comparator_witness = cmp

    let comparator = comparator
  end )

module Using_comparator = struct
  include Map.Using_comparator

  let of_hashtbl_exn ~comparator hashtbl =
    match of_iteri ~comparator ~iteri:(Hashtbl.iteri hashtbl) with
    | `Ok map -> map
    | `Duplicate_key key ->
      Error.create "Map.of_hashtbl_exn: duplicate key" key comparator.sexp_of_t |> Error.raise

  let tree_of_hashtbl_exn ~comparator hashtbl = to_tree (of_hashtbl_exn ~comparator hashtbl)

  let key_set ~comparator t =
    Set.Using_comparator.of_sorted_array_unchecked ~comparator (List.to_array (keys t))

  let key_set_of_tree ~comparator t = key_set ~comparator (of_tree ~comparator t)

  let of_key_set key_set ~f =
    of_sorted_array_unchecked ~comparator:(Set.comparator key_set)
      (Array.map (Set.to_array key_set) ~f:(fun key -> key, f key))

  let tree_of_key_set key_set ~f = to_tree (of_key_set key_set ~f)
end

module Accessors = struct
  include (
    Map.Using_comparator :
      Map.Accessors_generic
        with type ('a, 'b, 'c) access_options := ('a, 'b, 'c) Map.Without_comparator.t
        with type ('a, 'b, 'c) t := ('a, 'b, 'c) Map.t
        with type ('a, 'b, 'c) tree := ('a, 'b, 'c) Tree0.t
        with type 'k key := 'k
        with type 'c cmp := 'c )

  let key_set t = Using_comparator.key_set t ~comparator:(Using_comparator.comparator t)
end

let key_set t = Using_comparator.key_set ~comparator:(Using_comparator.comparator t) t

let of_key_set = Using_comparator.of_key_set

let hash_fold_direct = Using_comparator.hash_fold_direct

let comparator = Using_comparator.comparator

let comparator_s = Map.comparator_s

type 'k key = 'k

type 'c cmp = 'c

include (
  struct
    include Map

    let of_tree m = Map.Using_comparator.of_tree ~comparator:(to_comparator m)

    let to_tree = Map.Using_comparator.to_tree
  end :
    sig
      type ('a, 'b, 'c) t = ('a, 'b, 'c) Map.t

      include
        Map.Creators_and_accessors_generic
          with type ('a, 'b, 'c) create_options := ('a, 'b, 'c) Map.With_first_class_module.t
          with type ('a, 'b, 'c) access_options := ('a, 'b, 'c) Map.Without_comparator.t
          with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
          with type ('a, 'b, 'c) tree := ('a, 'b, 'c) Tree0.t
          with type 'k key := 'k key
          with type 'c cmp := 'c cmp
    end )

module Empty_without_value_restriction = Using_comparator.Empty_without_value_restriction

let merge_skewed = Map.merge_skewed

let of_hashtbl_exn m t = Using_comparator.of_hashtbl_exn ~comparator:(to_comparator m) t

module Creators (Key : Comparator.S1) : sig
  type ('a, 'b, 'c) t_ = ('a Key.t, 'b, Key.comparator_witness) t

  type ('a, 'b, 'c) tree = ('a, 'b, Key.comparator_witness) Tree0.t

  val t_of_sexp : (Sexp.t -> 'a Key.t) -> (Sexp.t -> 'b) -> Sexp.t -> ('a, 'b, _) t_

  include
    Creators_generic
      with type ('a, 'b, 'c) t := ('a, 'b, 'c) t_
      with type ('a, 'b, 'c) tree := ('a, 'b, 'c) tree
      with type 'a key := 'a Key.t
      with type 'a cmp := Key.comparator_witness
      with type ('a, 'b, 'c) create_options := ('a, 'b, 'c) Map.Without_comparator.t
      with type ('a, 'b, 'c) access_options := ('a, 'b, 'c) Map.Without_comparator.t
end = struct
  let comparator = Key.comparator

  type ('a, 'b, 'c) t_ = ('a Key.t, 'b, Key.comparator_witness) t

  type ('a, 'b, 'c) tree = ('a, 'b, Key.comparator_witness) Tree0.t

  module M_empty = Empty_without_value_restriction (Key)

  let empty = M_empty.empty

  let of_tree tree = Using_comparator.of_tree ~comparator tree

  let singleton k v = Using_comparator.singleton ~comparator k v

  let of_sorted_array_unchecked array = Using_comparator.of_sorted_array_unchecked ~comparator array

  let of_sorted_array array = Using_comparator.of_sorted_array ~comparator array

  let of_increasing_iterator_unchecked ~len ~f =
    Using_comparator.of_increasing_iterator_unchecked ~comparator ~len ~f

  let of_increasing_sequence seq = Using_comparator.of_increasing_sequence ~comparator seq

  let of_sequence seq = Using_comparator.of_sequence ~comparator seq

  let of_sequence_or_error seq = Using_comparator.of_sequence_or_error ~comparator seq

  let of_sequence_exn seq = Using_comparator.of_sequence_exn ~comparator seq

  let of_sequence_multi seq = Using_comparator.of_sequence_multi ~comparator seq

  let of_sequence_fold seq ~init ~f = Using_comparator.of_sequence_fold ~comparator seq ~init ~f

  let of_sequence_reduce seq ~f = Using_comparator.of_sequence_reduce ~comparator seq ~f

  let of_list_with_key list ~get_key = Using_comparator.of_list_with_key ~comparator list ~get_key

  let of_list_with_key_or_error list ~get_key =
    Using_comparator.of_list_with_key_or_error ~comparator list ~get_key

  let of_list_with_key_exn list ~get_key = Using_comparator.of_list_with_key_exn ~comparator list ~get_key

  let of_list_with_key_multi list ~get_key =
    Using_comparator.of_list_with_key_multi ~comparator list ~get_key

  let of_alist alist = Using_comparator.of_alist ~comparator alist

  let of_alist_or_error alist = Using_comparator.of_alist_or_error ~comparator alist

  let of_alist_exn alist = Using_comparator.of_alist_exn ~comparator alist

  let of_hashtbl_exn hashtbl = Using_comparator.of_hashtbl_exn ~comparator hashtbl

  let of_alist_multi alist = Using_comparator.of_alist_multi ~comparator alist

  let of_alist_fold alist ~init ~f = Using_comparator.of_alist_fold ~comparator alist ~init ~f

  let of_alist_reduce alist ~f = Using_comparator.of_alist_reduce ~comparator alist ~f

  let of_iteri ~iteri = Using_comparator.of_iteri ~comparator ~iteri

  let of_iteri_exn ~iteri = Using_comparator.of_iteri_exn ~comparator ~iteri

  let t_of_sexp k_of_sexp v_of_sexp sexp =
    Using_comparator.t_of_sexp_direct ~comparator k_of_sexp v_of_sexp sexp

  let of_key_set key_set ~f = Using_comparator.of_key_set key_set ~f

  let map_keys t ~f = Using_comparator.map_keys ~comparator t ~f

  let map_keys_exn t ~f = Using_comparator.map_keys_exn ~comparator t ~f

  let transpose_keys t = Using_comparator.transpose_keys ~comparator t
end

module Make_tree_S1 (Key : Comparator.S1) = struct
  open Tree0

  let comparator = Key.comparator

  let sexp_of_t = sexp_of_t

  let t_of_sexp a b c = t_of_sexp_direct a b c ~comparator

  let empty = empty_without_value_restriction

  let of_tree tree = tree

  let singleton a = singleton a ~comparator

  let of_sorted_array_unchecked a = of_sorted_array_unchecked a ~comparator

  let of_sorted_array a = of_sorted_array a ~comparator

  let of_increasing_iterator_unchecked ~len ~f = of_increasing_iterator_unchecked ~len ~f ~comparator

  let of_increasing_sequence seq = of_increasing_sequence ~comparator seq

  let of_sequence s = of_sequence s ~comparator

  let of_sequence_or_error s = of_sequence_or_error s ~comparator

  let of_sequence_exn s = of_sequence_exn s ~comparator

  let of_sequence_multi s = of_sequence_multi s ~comparator

  let of_sequence_fold s ~init ~f = of_sequence_fold s ~init ~f ~comparator

  let of_sequence_reduce s ~f = of_sequence_reduce s ~f ~comparator

  let of_alist a = of_alist a ~comparator

  let of_alist_or_error a = of_alist_or_error a ~comparator

  let of_alist_exn a = of_alist_exn a ~comparator

  let of_hashtbl_exn a = Using_comparator.tree_of_hashtbl_exn a ~comparator

  let of_alist_multi a = of_alist_multi a ~comparator

  let of_alist_fold a ~init ~f = of_alist_fold a ~init ~f ~comparator

  let of_alist_reduce a ~f = of_alist_reduce a ~f ~comparator

  let of_list_with_key l ~get_key = of_list_with_key l ~get_key ~comparator

  let of_list_with_key_or_error l ~get_key = of_list_with_key_or_error l ~get_key ~comparator

  let of_list_with_key_exn l ~get_key = of_list_with_key_exn l ~get_key ~comparator

  let of_list_with_key_multi l ~get_key = of_list_with_key_multi l ~get_key ~comparator

  let of_iteri ~iteri = of_iteri ~iteri ~comparator

  let of_iteri_exn ~iteri = of_iteri_exn ~iteri ~comparator

  let of_key_set = Using_comparator.tree_of_key_set

  let to_tree t = t

  let invariants a = invariants a ~comparator

  let is_empty a = is_empty a

  let length a = length a

  let set a ~key ~data = set a ~key ~data ~comparator

  let add a ~key ~data = add a ~key ~data ~comparator

  let add_exn a ~key ~data = add_exn a ~key ~data ~comparator

  let add_multi a ~key ~data = add_multi a ~key ~data ~comparator

  let remove_multi a b = remove_multi a b ~comparator

  let find_multi a b = find_multi a b ~comparator

  let change a b ~f = change a b ~f ~comparator

  let update a b ~f = update a b ~f ~comparator

  let find_exn a b = find_exn a b ~comparator

  let find a b = find a b ~comparator

  let remove a b = remove a b ~comparator

  let mem a b = mem a b ~comparator

  let iter_keys = iter_keys

  let iter = iter

  let iteri = iteri

  let iteri_until = iteri_until

  let iter2 a b ~f = iter2 a b ~f ~comparator

  let map = map

  let mapi = mapi

  let fold = fold

  let fold_until = fold_until

  let fold_right = fold_right

  let fold2 a b ~init ~f = fold2 a b ~init ~f ~comparator

  let filter_keys a ~f = filter_keys a ~f

  let filter a ~f = filter a ~f

  let filteri a ~f = filteri a ~f

  let filter_map a ~f = filter_map a ~f

  let filter_mapi a ~f = filter_mapi a ~f

  let partition_mapi t ~f = partition_mapi t ~f

  let partition_map t ~f = partition_map t ~f

  let partitioni_tf t ~f = partitioni_tf t ~f

  let partition_tf t ~f = partition_tf t ~f

  let combine_errors t = combine_errors t ~comparator

  let compare_direct a b c = compare_direct a b c ~comparator

  let equal a b c = equal a b c ~comparator

  let keys = keys

  let data = data

  let to_alist = to_alist

  let symmetric_diff a b ~data_equal = symmetric_diff a b ~data_equal ~comparator

  let fold_symmetric_diff a b ~data_equal ~init ~f =
    fold_symmetric_diff a b ~data_equal ~f ~init ~comparator

  let merge a b ~f = merge a b ~f ~comparator

  let merge_skewed a b ~combine = merge_skewed a b ~combine ~comparator

  let min_elt = min_elt

  let min_elt_exn = min_elt_exn

  let max_elt = max_elt

  let max_elt_exn = max_elt_exn

  let for_all = for_all

  let for_alli = for_alli

  let exists = exists

  let existsi = existsi

  let count = count

  let counti = counti

  let split a b = split a b ~comparator

  let split_le_gt a b = split_le_gt a b ~comparator

  let split_lt_ge a b = split_lt_ge a b ~comparator

  let append ~lower_part ~upper_part = append ~lower_part ~upper_part ~comparator

  let subrange t ~lower_bound ~upper_bound = subrange t ~lower_bound ~upper_bound ~comparator

  let fold_range_inclusive t ~min ~max ~init ~f = fold_range_inclusive t ~min ~max ~init ~f ~comparator

  let range_to_alist t ~min ~max = range_to_alist t ~min ~max ~comparator

  let closest_key a b c = closest_key a b c ~comparator

  let nth = nth

  let nth_exn = nth_exn

  let rank a b = rank a b ~comparator

  let to_sequence ?order ?keys_greater_or_equal_to ?keys_less_or_equal_to t =
    to_sequence ~comparator ?order ?keys_greater_or_equal_to ?keys_less_or_equal_to t

  let binary_search t ~compare how v = binary_search ~comparator t ~compare how v

  let binary_search_segmented t ~segment_of how = binary_search_segmented ~comparator t ~segment_of how

  let binary_search_subrange t ~compare ~lower_bound ~upper_bound =
    binary_search_subrange ~comparator t ~compare ~lower_bound ~upper_bound

  module Make_applicative_traversals (A : Applicative.Lazy_applicative) = struct
    module Traversals = Make_applicative_traversals (A)

    let mapi = Traversals.mapi

    let filter_mapi = Traversals.filter_mapi
  end

  let key_set t = Using_comparator.key_set_of_tree ~comparator t

  let map_keys t ~f = map_keys t ~f ~comparator

  let map_keys_exn t ~f = map_keys_exn t ~f ~comparator

  let transpose_keys t = transpose_keys ~comparator ~comparator t
end

module Make_tree_plain (Key : sig
  type t [@@deriving sexp_of]

  include Comparator.S with type t := t
end) =
struct
  module Key_S1 = Comparator.S_to_S1 (Key)
  include Make_tree_S1 (Key_S1)

  type +'v t = (Key.t, 'v, Key.comparator_witness) Tree0.t

  let sexp_of_t sexp_of_v t = sexp_of_t Key.sexp_of_t sexp_of_v [%sexp_of: _] t

  module Provide_of_sexp
    (X : sig
      type t [@@deriving of_sexp]
    end
    with type t := Key.t) =
  struct
    let t_of_sexp v_of_sexp sexp = t_of_sexp X.t_of_sexp v_of_sexp sexp
  end
end

module Make_tree (Key : sig
  type t [@@deriving sexp]

  include Comparator.S with type t := t
end) =
struct
  include Make_tree_plain (Key)
  include Provide_of_sexp (Key)
end

module type Key_plain = sig
  type t [@@deriving sexp_of]

  val compare : t -> t -> int
end

module type Key = sig
  type t [@@deriving sexp]

  val compare : t -> t -> int
end

module Make_plain_using_comparator (Key : sig
  type t [@@deriving sexp_of]

  include Comparator.S with type t := t
end) =
struct
  module Key = Key
  module Key_S1 = Comparator.S_to_S1 (Key)
  include Creators (Key_S1)

  type key = Key.t

  type ('a, 'b, 'c) map = ('a, 'b, 'c) t

  type 'v t = (key, 'v, Key.comparator_witness) map

  include Accessors

  let compare cmpv t1 t2 = compare_direct cmpv t1 t2

  let sexp_of_t sexp_of_v t = Using_comparator.sexp_of_t Key.sexp_of_t sexp_of_v [%sexp_of: _] t

  module Provide_of_sexp
    (Key : sig
      type t [@@deriving of_sexp]
    end
    with type t := Key.t) =
  struct
    let t_of_sexp v_of_sexp sexp = t_of_sexp Key.t_of_sexp v_of_sexp sexp
  end

  module Provide_hash (Key' : Hasher.S with type t := Key.t) = struct
    let hash_fold_t (type a) hash_fold_data state (t : a t) =
      Using_comparator.hash_fold_direct Key'.hash_fold_t hash_fold_data state t
  end
end

module Make_plain (Key : Key_plain) = Make_plain_using_comparator (struct
  include Key
  include Comparator.Make (Key)
end)

module Make_using_comparator (Key_sexp : sig
  type t [@@deriving sexp]

  include Comparator.S with type t := t
end) =
struct
  include Make_plain_using_comparator (Key_sexp)
  module Key = Key_sexp
  include Provide_of_sexp (Key)

  module _ = struct
    include Tree0
    include Provide_of_sexp (Key)
  end
end

module Make (Key : Key) = Make_using_comparator (struct
  include Key
  include Comparator.Make (Key)
end)

module Tree = struct
  include Tree0

  let of_hashtbl_exn = Using_comparator.tree_of_hashtbl_exn

  let key_set = Using_comparator.key_set_of_tree

  let of_key_set = Using_comparator.tree_of_key_set
end
