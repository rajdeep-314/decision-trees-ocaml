(*
    Datast:
        [ Add description here. ]

*)

open Utils


type attr = Attr of string          (* Attribute type.  *)
type lbl = Lbl of string            (* Label type.      *)

type aval = Aval of string          (* Attribute values type.       *)
type lval = Lval of string          (* Label values (classes) type. *)


type t = {
    mutable n_attrs : int;
    mutable size : int;
    mutable attrs : attr list;
    mutable lname : lbl;
    data : (attr, aval list) Hashtbl.t;
    mutable vals: lval list
}

(* The size of the datset. *)
let size (ds : t) : int = ds.size

(* The number of attributes of the dataset. *)
let n_attrs (ds : t) : int = ds.n_attrs


(* The n'th value of the attribute atb. *)
let nth_of_attr (ds : t) (atb : attr) (n : int) : aval =
    List.nth (Hashtbl.find ds.data atb) n

(* The n'th attribute of the dataset. *)
let nth_attr (ds : t) (n : int) : attr = List.nth ds.attrs n

(* The n'th label value. *)
let nth_lval (ds : t) (n : int) : lval = List.nth ds.vals n

(* The n'th row of the dataset. *)
let nth_row (ds : t) (n : int) : aval list * lval =
    (List.fold_left (fun x y -> x @ [nth_of_attr ds y n]) [] ds.attrs, nth_lval ds n)


(* Returns a list of the attribute values corresponding
   to the attribute atb.
   Causes an exception for an invalid attribute. *)
let list_of_attr (ds : t) (atb : attr) : aval list = Hashtbl.find ds.data atb

(* List of label values from ds. *)
let list_of_lvals (ds : t) : lval list = ds.vals

(* Copies a dataset and returns another one practically
   identical to it. *)
let cpy (ds : t) : t =
    { n_attrs = ds.n_attrs;
      size = ds.size;
      attrs = ds.attrs;
      lname = ds.lname;
      data = ds.data;
      vals = ds.vals }

(* Removes the attribute atb from ds and returns a new dataset. *)
let rem_attr (ds : t) (atb : attr) : t =
    let nds = cpy ds in
    let () = Hashtbl.remove nds.data atb in
    let () = nds.n_attrs <- nds.n_attrs - 1 in
    let () = nds.attrs <- List.filter (fun x -> not (x = atb)) nds.attrs in
    nds


(* Filters and returns the label values corresponding to
   those entries in which atb has the value av.
   NOTE:    Doesn't guarantee any order. *)
let filter (ds : t) (atb : attr) (av : aval) : lval list =
    let l = list_pdt (list_of_attr ds atb) (list_of_lvals ds) in
    let filtered = List.filter (fun (x, _) -> x = av) l in
    List.map snd filtered


(* Some functions to transform Parser.t values to attr,
   aval and lval values. *)
let attr_of_tok (Parser.Tok tok) = Attr tok
let lbl_of_tok (Parser.Tok tok) = Lbl tok
let aval_of_tok (Parser.Tok tok) = Aval tok
let lval_of_tok (Parser.Tok tok) = Lval tok


(* Printing functions. *)
let print_attr (Attr s) (term : string) : unit = Printf.printf "%s%s" s term
let print_attr_list (l : attr list) (sep : string) (term : string) : unit =
        let () = List.iter (fun x -> print_attr x sep) l
        in Printf.printf "%s" term

let print_aval (Aval s) (term : string) : unit = Printf.printf "%s%s" s term
let print_aval_list (l : aval list) (sep : string) (term : string) : unit =
        let () = List.iter (fun x -> print_aval x sep) l
        in Printf.printf "%s" term

let print_data_entry (a : attr) (l : aval list) : unit =
        print_attr a ": ";
        print_aval_list l " " "\n"

let print_lval (Lval v) (term : string) : unit = Printf.printf "%s%s" v term
let print_lvals (ds : t) : unit =
        List.iter (fun v -> print_lval v " ") ds.vals; print_endline ""


(* Printing the entire dataset, attribute-wise. *)
let print_by_attr (ds : t) : unit =
    Printf.printf "Size: %d\n" ds.size;
    Printf.printf "# Attributes = %d\nAttributes\t:\t| " ds.n_attrs;
    print_attr_list ds.attrs " | " "\n";
    Hashtbl.iter print_data_entry ds.data;
    Printf.printf "\n%s: " ((fun (Lbl s) -> s) ds.lname);
    print_lvals ds


let print_nth_row (ds : t) (n : int) (sep : string) (term : string) : unit =
    let row, value = nth_row ds n in
    Printf.printf "%d\t\t|\t" (n + 1);
    print_aval_list row sep "";
    print_lval value term

(* A list with the first n natural numbers greater than 0. *)
let rec nat_nums (n : int) : int list = if n = 0 then [] else (nat_nums (n-1)) @ [n]

(* Printing the entire dataset, row-wise, in a CSV-like format. *)
let print (ds : t) : unit =
    Printf.printf "Dataset size = %d\nNo. of attributes = %d\n\nSr. No.\t|\t" ds.size ds.n_attrs;
    print_attr_list ds.attrs ", " "";
    Printf.printf "%s\n" ((fun (Lbl s) -> s) ds.lname);
    List.iter (fun n -> print_nth_row ds (n - 1) ", " "\n") (nat_nums ds.size)


(* Constructing the data set from the parsed CSV. *)

let rec split_last_helper (l : 'a list) (acc : 'a list) : 'a list * 'a =
    match l with
    | [] -> failwith "Insufficient size for split_last."
    | last :: [] -> (acc, last)
    | x :: xs -> split_last_helper xs (acc @ [x])


let split_last (l : 'a list) : 'a list * 'a =
    split_last_helper l []


let extract_attributes_label (l : Parser.t list) : attr list * lbl =
    match l with
    | [] -> failwith "Invalid format."
    | _ :: [] -> failwith "Invalid format."
    | _ -> let first, last = split_last l in
            (List.map attr_of_tok first, lbl_of_tok last)


(* This adds the new attribute value in "revsere order" in the
   hash table, which is then handled inside some other function.
   Also, the hash table should be initialized with the attributes. *)
let rec process_row (l : Parser.t list) (atts : attr list) (ds : t) : unit =
    match l, atts with
    | tok :: toks, atb :: atbs ->
            let old_list = Hashtbl.find ds.data atb in
            Hashtbl.replace ds.data atb (aval_of_tok tok :: old_list);
            process_row toks atbs ds
    | ltok :: [], [] ->
            ds.vals <- (lval_of_tok ltok) :: ds.vals;
            ds.size <- ds.size + 1
    | _, _ -> ()            (* this shouldn't happen if the formatting is correct. *)


let process_rows (rows : Parser.t list list) (ds : t) : unit =
    List.iter (fun row -> process_row row ds.attrs ds) rows


(* Initializes a hash table with the attributes present in alist. *)
let rec hashtable_init (tbl : (attr, aval list) Hashtbl.t) (alist : attr list) : unit =
    match alist with
    | [] -> ()
    | atb :: atbs -> Hashtbl.add tbl atb [];
                    hashtable_init tbl atbs


(* Creates a new dataset with atts - the given attribute list, and
   lname - the name of the labels. *)
let new_dataset (atts : attr list) (lname : lbl) : t =
    let tbl = Hashtbl.create 100 in
    let () = hashtable_init tbl atts in
    { n_attrs = List.length atts; size = 0; attrs = atts; lname = lname; data = tbl; vals = [] }


let reverse_list (ds : t) (atb : attr) : unit =
    let rev_list = List.rev (Hashtbl.find ds.data atb) in
    Hashtbl.replace ds.data atb rev_list


(* Creates a new dataset out of a parsed CSV.
   The type of a parsed CSV is Parser.t list list. *)
let of_csv (csv : Parser.t list list) : t =
    match csv with
    | [] -> failwith "Invalid format."
    | _ :: [] -> failwith "Invalid format - No values, only header."
    | header :: tail ->
            let atts, lname = extract_attributes_label header in
            let ds = new_dataset atts lname in
            let () = process_rows tail ds in
            let _ = List.map (fun atb -> reverse_list ds atb) atts in
            let () = ds.vals <- List.rev ds.vals in
            ds

