(* This file consists of code taken from 2 places: OCaml's standard library and Jane Street's Base library [1]
   It contains just the necessary code to implement the Map functions used by tdigest.ml, with the key specialized to Float.
   Additionally, it contains the binary_search function from Jane Street's Base's Map module (and its associated helpers) [2],
   adapted to work on the standard library Map type.

   [1]: https://github.com/ocaml/ocaml/blob/a6b1747b2b288dcb723cf3e766ad943324351e5a/stdlib/map.ml
   [2]: https://github.com/janestreet/base/blob/e8458a58556d407ce679f1856f8a5ecf2ecd26f3/src/map.ml

   The copyright and license information of both is included below, in the same order.
*)
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Apache 2.0 license. See ../THIRD-PARTY.txt  *)
(*  for details.                                                       *)
(*                                                                     *)
(***********************************************************************)

type 'a t =
  | Empty
  | Node of {
      l: 'a t;
      v: float;
      d: 'a;
      r: 'a t;
      h: int;
    }

let height = function
| Empty -> 0
| Node { h; _ } -> h

let create l x d r =
  let hl = height l
  and hr = height r in
  Node { l; v = x; d; r; h = (if hl >= hr then hl + 1 else hr + 1) }

let empty = Empty

let is_empty = function
| Empty -> true
| _ -> false

let bal l x d r =
  let hl =
    match l with
    | Empty -> 0
    | Node { h; _ } -> h
  in
  let hr =
    match r with
    | Empty -> 0
    | Node { h; _ } -> h
  in
  if hl > hr + 2
  then begin
    match l with
    | Empty -> invalid_arg "Map.bal"
    | Node { l = ll; v = lv; d = ld; r = lr; _ } ->
      if height ll >= height lr
      then create ll lv ld (create lr x d r)
      else begin
        match lr with
        | Empty -> invalid_arg "Map.bal"
        | Node { l = lrl; v = lrv; d = lrd; r = lrr; _ } ->
          create (create ll lv ld lrl) lrv lrd (create lrr x d r)
      end
  end
  else if hr > hl + 2
  then begin
    match r with
    | Empty -> invalid_arg "Map.bal"
    | Node { l = rl; v = rv; d = rd; r = rr; _ } ->
      if height rr >= height rl
      then create (create l x d rl) rv rd rr
      else begin
        match rl with
        | Empty -> invalid_arg "Map.bal"
        | Node { l = rll; v = rlv; d = rld; r = rlr; _ } ->
          create (create l x d rll) rlv rld (create rlr rv rd rr)
      end
  end
  else Node { l; v = x; d; r; h = (if hl >= hr then hl + 1 else hr + 1) }

let rec add x data = function
| Empty -> Node { l = Empty; v = x; d = data; r = Empty; h = 1 }
| Node { l; v; d; r; h } as m ->
  let c = Float.compare x v in
  if c = 0
  then if d == data then m else Node { l; v = x; d = data; r; h }
  else if c < 0
  then (
    let ll = add x data l in
    if l == ll then m else bal ll v d r )
  else (
    let rr = add x data r in
    if r == rr then m else bal l v d rr )

let rec min_binding = function
| Empty -> raise Not_found
| Node { l = Empty; v; d; _ } -> v, d
| Node { l; _ } -> min_binding l

let rec min_binding_opt = function
| Empty -> None
| Node { l = Empty; v; d; _ } -> Some (v, d)
| Node { l; _ } -> min_binding_opt l

let rec max_binding_opt = function
| Empty -> None
| Node { v; d; r = Empty; _ } -> Some (v, d)
| Node { r; _ } -> max_binding_opt r

let rec remove_min_binding = function
| Empty -> invalid_arg "Map.remove_min_elt"
| Node { l = Empty; r; _ } -> r
| Node { l; v; d; r; _ } -> bal (remove_min_binding l) v d r

let merge t1 t2 =
  match t1, t2 with
  | Empty, t -> t
  | t, Empty -> t
  | _, _ ->
    let x, d = min_binding t2 in
    bal t1 x d (remove_min_binding t2)

let rec remove x = function
| Empty -> Empty
| Node { l; v; d; r; _ } as m ->
  let c = Float.compare x v in
  if c = 0
  then merge l r
  else if c < 0
  then (
    let ll = remove x l in
    if l == ll then m else bal ll v d r )
  else (
    let rr = remove x r in
    if r == rr then m else bal l v d rr )

let rec cardinal = function
| Empty -> 0
| Node { l; r; _ } -> cardinal l + 1 + cardinal r

let rec iter f = function
| Empty -> ()
| Node { l; v; d; r; _ } ->
  iter f l;
  f v d;
  iter f r

let rec map f = function
| Empty -> Empty
| Node { l; v; d; r; h } ->
  let l' = map f l in
  let d' = f d in
  let r' = map f r in
  Node { l = l'; v; d = d'; r = r'; h }

let rec fold f m accu =
  match m with
  | Empty -> accu
  | Node { l; v; d; r; _ } -> fold f r (f v d (fold f l accu))

let rec find_first_satisfying t ~f =
  match t with
  | Empty -> None
  | Node { l; v = k; d = v; r; _ } ->
    if f ~key:k ~data:v
    then (
      match find_first_satisfying l ~f with
      | None -> Some (k, v)
      | Some _ as x -> x )
    else find_first_satisfying r ~f

let rec find_last_satisfying t ~f =
  match t with
  | Empty -> None
  | Node { l; v = k; d = v; r; _ } ->
    if f ~key:k ~data:v
    then (
      match find_last_satisfying r ~f with
      | None -> Some (k, v)
      | Some _ as x -> x )
    else find_last_satisfying l ~f

let binary_search t ~compare how v =
  match how with
  | `Last_strictly_less_than ->
    find_last_satisfying t ~f:(fun ~key ~data -> compare ~key ~data v < 0) [@nontail]
  | `Last_less_than_or_equal_to ->
    find_last_satisfying t ~f:(fun ~key ~data -> compare ~key ~data v <= 0) [@nontail]
  | `First_equal_to -> (
    match find_first_satisfying t ~f:(fun ~key ~data -> compare ~key ~data v >= 0) with
    | Some (key, data) as pair when compare ~key ~data v = 0 -> pair
    | None
     |Some _ ->
      None )
  | `Last_equal_to -> (
    match find_last_satisfying t ~f:(fun ~key ~data -> compare ~key ~data v <= 0) with
    | Some (key, data) as pair when compare ~key ~data v = 0 -> pair
    | None
     |Some _ ->
      None )
  | `First_greater_than_or_equal_to ->
    find_first_satisfying t ~f:(fun ~key ~data -> compare ~key ~data v >= 0) [@nontail]
  | `First_strictly_greater_than ->
    find_first_satisfying t ~f:(fun ~key ~data -> compare ~key ~data v > 0) [@nontail]

type 'a enumeration =
  | End
  | More of float * 'a * 'a t * 'a enumeration

let rec snoc_enum s e =
  match s with
  | Empty -> e
  | Node { l; v; d; r; _ } -> snoc_enum r (More (v, d, l, e))

let rec rev_seq_of_enum_ c () =
  match c with
  | End -> Seq.Nil
  | More (k, v, t, rest) -> Seq.Cons ((k, v), rev_seq_of_enum_ (snoc_enum t rest))

let to_rev_seq c = rev_seq_of_enum_ (snoc_enum c End)
