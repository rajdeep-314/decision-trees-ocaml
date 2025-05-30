(*
    utils.ml

    For calculations, primarily.
    Has some other useful utility functions too.
*)


(* The Cartesian-esque product of two lists. *)
let rec list_pdt (l1 : 'a list) (l2 : 'b list) : ('a * 'b) list =
    match l1, l2 with
    | x :: xs, y :: ys -> (x, y) :: (list_pdt xs ys)
    | _ -> []


(* A list of values of a hash table. *)
let list_of_values (tbl : ('a, 'b) Hashtbl.t) : 'b list =
    List.of_seq (Hashtbl.to_seq_values tbl)


(* Weighted average with the given weights and values. *)
let w_avg (weights : int list) (values : float list) : float =
    let total = float_of_int (List.fold_left ( + ) 0 weights) in
    let ratios = List.map (fun x -> (float_of_int x) /. total) weights in
    List.fold_left ( +. ) 0. (List.map2 ( *. ) ratios values)


(* Logarithm with base 2. *)
let log2 k = (log k) /. (log 2.)


(* Helper function for freq_dist. *)
let rec freq_dist_helper (l : 'a list) (tbl : ('a, int) Hashtbl.t) : unit =
    match l with
    | [] -> ()
    | x :: xs ->
            if Hashtbl.mem tbl x then
                let old_freq = Hashtbl.find tbl x in
                Hashtbl.replace tbl x (old_freq + 1)
            else
                Hashtbl.add tbl x 1;
            freq_dist_helper xs tbl

(* Generates the frequency distribution table for values in a
   list. A hash table is used to represent this table. *)
let freq_dist (l : 'a list) : ('a, int) Hashtbl.t =
    let tbl = Hashtbl.create 100 in
    let () = freq_dist_helper l tbl in
    tbl


(* Returns a list of the unique values in l. *)
let uniques (l : 'a list) : 'a list =
    let tbl = freq_dist l in
    List.of_seq (Hashtbl.to_seq_keys tbl)


(* Returns a list of frequencies of the values in l.
   NOTE:    No order is guaranteed. *)
let frequencies (l : 'a list) : int list =
    let tbl = freq_dist l in
    list_of_values tbl


