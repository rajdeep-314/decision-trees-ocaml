(*
    utils.ml

    For calculations, primarily.
    Has some other useful utility functions too.
*)


(* Helper function for indices_with_val.
   acc is the list of "accumulated indices" and ind
   is the starting index. *)
let rec indices_with_val_helper (l : 'a list) (v : 'a) (acc : int list) (ind : int) : int list =
    match l with
    | [] -> acc
    | x :: xs ->
            let new_ind = ind + 1 in
            if x = v then indices_with_val_helper xs v (ind :: acc) new_ind
            else indices_with_val_helper xs v acc new_ind


(* Returns the indices of l that have value v, in the form
   of an int list.
   NOTE:    Order is not guaranteed. *)
let indices_with_val (l : 'a list) (v : 'a) : int list =
    indices_with_val_helper l v [] 0


(* Returns the elements of l, the indices of which are not in inds. *)
let list_index_filter (l : 'a list) (inds : int list) : 'a list =
    List.filteri (fun x _ -> List.mem x inds) l


(* Removes the element at index n from l and returns
   the new list.
   NOTE:    Returns the same list for an invalid index. *)
let rec list_rem (l : 'a list) (n : int) : 'a list =
    if n < 0 then l else
    if n = 0 then
        match l with
        | [] -> []
        | _ :: xs -> xs
    else
        match l with
        | [] -> []
        | x :: xs -> x :: list_rem xs (n - 1)


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


(* Returns x such that (x, y) is a pair in l and y is the
   maximum of all snd values, where these values are ints. *)
let max_snd_int (l : ('a * int) list) : 'a =
    let sorted = List.sort (fun (_, b) (_, d) -> -(Int.compare b d)) l in
    fst (List.nth sorted 0)

(* Essentially the same as max_snd_int but with the snd
   values being floats. *)
let max_snd_float (l : ('a * float) list) : 'a =
    let sorted = List.sort (fun (_, b) (_, d) -> -(Float.compare b d)) l in
    fst (List.nth sorted 0)

(* Returns the key of tbl with the maximum integer value. *)
let htbl_mode (tbl : ('a, int) Hashtbl.t) : 'a =
    let fl = List.of_seq (Hashtbl.to_seq tbl) in
    max_snd_int fl


(* The value from l with the maximum frequency. *)
let list_mode (l : 'a list) : 'a =
    htbl_mode (freq_dist l)


(* True if all the elements in l are the same,
   and false otherwise. *)
let rec is_homogeneous (l : 'a list) : bool =
    match l with
    | [] -> true
    | _ :: [] -> true
    | x :: (y :: _ as tail) -> x = y && is_homogeneous tail


