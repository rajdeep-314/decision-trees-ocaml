(*
    query.ml

    Type and functions for queries (over a decision tree).
*)


module Ds = Dataset


(* A query is a list of attributes and attribute values. *)
type t = Qr of (Ds.attr * Ds.aval) list


(* Helper functions for rem. *)
let rec rem_helper (l : ('a * 'b) list) (v : 'a) : ('a * 'b) list =
    match l with
    | [] -> []
    | (x, y) :: xs ->
            if x = v then rem_helper xs v
            else (x, y) :: (rem_helper xs v)


(* Remove all instances of (atb, _) from ql. *)
let rem (Qr ql : t) (atb : Ds.attr) : t =
    Qr (rem_helper ql atb)


(* Makes a query out of the n'th row of the dataset ds. *)
let of_row (ds : Ds.t) (n : int) : t =
    let (l, _) = Ds.nth_row ds n in      (* the n'th row. *)
    Qr (List.map2 (fun x y -> (x ,y)) ds.attrs l)


