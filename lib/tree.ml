(*
    dtree.ml

    Decision tree type and functions for the same
*)


open Utils
module Ds = Dataset             (* Will probably remove this later. *)


type t =
    | Node of Ds.attr * ((Ds.aval * t) list)            (* A split node in the decision tree. *)
    | Leaf of Ds.lval                                   (* A leaf node in the decision tree. *)



(* The ID3 algorithm - iterative dichotomizer 3. *)
let rec id3 (ds : Ds.t) : t =
    let lvals_list = Ds.list_of_lvals ds in
    if Ds.nattrs ds = 0 then Leaf (list_mode lvals_list)
    else if is_homogeneous lvals_list then Leaf (List.nth lvals_list 0)
    else
        let attr_gain = List.map (fun atb -> (atb, Heur.gain ds atb)) ds.attrs in
        let best_attr = max_snd_float attr_gain in
        let alist = Ds.list_of_attr ds best_attr in
        let attr_vals = uniques alist in                (* The unique values of best_attr. *)
        Node (best_attr, (List.map (
            fun v -> let nds = Ds.attr_filter ds best_attr v in
                if Ds.is_empty nds then (v, Leaf (list_mode lvals_list))
                else (v, id3 nds)
        ) attr_vals))


(* Helper function for print_p. *)
let rec print_p_helper (tree : t) : unit =
    match tree with
    | Leaf v -> Printf.printf "( Leaf %s )" (Ds.string_of_lval v)
    | Node (a, l) ->
            Printf.printf "Node ( %s, [ " (Ds.string_of_attr a);
            List.iter (fun (x, y) ->
                Printf.printf "( %s, " (Ds.string_of_aval x); print_p_helper y; Printf.printf ")") l;
            Printf.printf "] )"

(* A primiive printing function. *)
let print_p (tree : t) : unit =
    print_p_helper tree; print_endline ""


(* Prints the root of the tree. *)
let print_root (tree : t) : unit =
    match tree with
    | Leaf l -> Printf.printf "%s" (Ds.string_of_lval l)
    | Node (n, _) -> Printf.printf "%s" (Ds.string_of_attr n)


module Q = Query                (* Will probably remove this later. *)


(* Predicts a label value based on the query.
   Returns None in case of some issues (invalid attribute, for example). *)
let rec apply_opt (tree : t) (Q.Qr ql as q: Q.t) : Ds.lval option =
    match tree with
    | Leaf l -> Some l
    | Node (n, l) ->
            if list_fst_mem ql n = false then None          (* Incomplete query. *)
            else
                apply_opt (list_snd_of_fst l (list_snd_of_fst ql n)) (Q.rem q n)


(* Same as apply_opt, but instead of returning None, it raises
   an exception. *)
let apply (tree : t) (q : Q.t) : Ds.lval =
    match apply_opt tree q with
    | None -> failwith "Invalid arguments passed to apply."
    | Some l -> l


(* Test the performance of a tree on the data that it was
   trained on.
   NOTE:    The accuracy SHOULD be 100%. *)
let test_on_ds (tree : t) (ds : Ds.t) : float =
    let indices = nat_nums ds.size in
    let queries = List.map (fun i -> Q.of_row ds i) indices in
    let predictions = List.map (fun q -> apply tree q) queries in
    let actual = List.rev ds.vals in
    list_accuracy predictions actual


