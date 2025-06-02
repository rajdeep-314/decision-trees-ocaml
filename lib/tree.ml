(*
    dtree.ml

    Decision tree type and functions for the same
*)


(* open Utils *)
module Ds = Dataset             (* Will probably remove this later. *)


type t =
    | Node of Ds.attr * ((Ds.aval * t) list)            (* A split node in the decision tree. *)
    | Leaf of Ds.lval                                   (* A leaf node in the decision tree. *)



(* The ID3 algorithm - iterative dichotomizer 3. *)
(* let rec id3 (ds : Ds.t) : t = *)
(*     let lvals_list = Ds.list_of_lvals ds in *)
(*     if Ds.nattrs ds = 0 then Leaf (list_mode lvals_list) *)
(*     else if is_homogeneous lvals_list then Leaf (List.nth lvals_list 0) *)
(*     else *)
(*         let attr_gain = List.map (fun atb -> (atb, Heur.gain ds atb)) ds.attrs in *)
(*         let best_attr = max_snd_float attr_gain in *)
(*         let alist = Ds.list_of_attr ds best_attr in *)
(*         let attr_vals = uniques alist in                (* The unique values of best_attr. *) *)
(*         () *)


