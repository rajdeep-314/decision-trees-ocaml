(*
    heur.ml:

    This module primarily contains functions regarding the heuristic
    being employed to split attributes in the decision tree.

    Currently, the heuristic being used is information gain, calculated
    as follows:
        $$G(I, x_i) = H(I) - \sum_{v \in \mathrm{values}(x_i)} \dfrac{|I_v|}{|I|} H(I_v)$$
    where $H$ is the entropy function, $I$ contains the instances/examples, and
    $I_v$ contains the examples that are consistent with $x_i = v$.
*)


open Utils

module Ds = Dataset             (* Will probably remove this later. *)


(* The entropy of a list of ratios is the sum of
   these terms for each of the ratios. *)
let entropy_term (r : float) : float =
    if r = 0. then 0. else
        (-1.) *. r *. (log2 r)


(* Gives the entropy of a list. *)
let entropy_of_list (l : 'a list) : float =
    let len = float_of_int (List.length l) in
    let tbl = freq_dist l in
    let freqs = list_of_values tbl in
    let ratios = List.map (fun x -> (float_of_int x) /. len) freqs in
    List.fold_left (fun oen r -> oen +. (entropy_term r)) 0. ratios


let gain (ds : Ds.t) (atb : Ds.attr) : float =
    let alist = Ds.list_of_attr ds atb in
    let vals = uniques alist in
    let val_lvals = List.map (fun x -> Ds.filter ds atb x) vals in
    let val_entropies = List.map entropy_of_list val_lvals in
    let weights = frequencies alist in
    let i_entropy = entropy_of_list (Ds.list_of_lvals ds) in
    i_entropy -. (w_avg weights val_entropies)


