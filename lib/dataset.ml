(*
    Datast:
        [ Add description here. ]

*)


type attr = Attr of string          (* Attribute type.  *)
(* type lbl = Label of string          (* Label type.      *) *)

type aval = Aval of string          (* Attribute values type.       *)
type lval = Lval of string          (* Label values (classes) type. *)


type t = {
    n_attrs : int;
    attrs : attr list;
    data : (attr, aval list) Hashtbl.t;
    mutable vals: lval list
}


(* Some functions to transform Parser.t values to attr,
   aval and lval values. *)
let attr_of_tok (Parser.Tok tok) = Attr tok
let aval_of_tok (Parser.Tok tok) = Aval tok
let lval_of_tok (Parser.Tok tok) = Lval tok


(* Some helper functions. *)
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

(* Printing the dataset. *)
let print (ds : t) : unit =
    Printf.printf "# Attributes = %d\nAttributes\t:\t| " ds.n_attrs;
    print_attr_list ds.attrs " | " "\n";
    Hashtbl.iter print_data_entry ds.data



(* Constructing the data set from the parsed CSV. *)

let rec remove_last (l : 'a list) : 'a list =
    match l with
    | [] -> []
    | _ :: [] -> []
    | x :: xs -> x :: remove_last xs


let extract_attributes (l : Parser.t list) : attr list =
    match l with
    | [] -> failwith "Invalid format."
    | _ :: [] -> failwith "Invalid format."
    | _ -> remove_last (List.map attr_of_tok l)


(* This adds the new attribute value in "revsere order" in the
   hash table, which is then handled inside some other function.
   Also, the hash table should be initialized with the attributes. *)
let rec process_row (l : Parser.t list) (atts : attr list) (ds : t) : unit =
    match l, atts with
    | tok :: toks, atb :: atbs ->
            let old_list = Hashtbl.find ds.data atb in
            Hashtbl.replace ds.data atb (aval_of_tok tok :: old_list);
            process_row toks atbs ds
    | ltok :: [], [] -> ds.vals <- (lval_of_tok ltok) :: ds.vals
    | _, _ -> ()            (* this shouldn't happen if the formatting is correct. *)


let process_rows (rows : Parser.t list list) (ds : t) : unit =
    List.iter (fun row -> process_row row ds.attrs ds) rows


(* Initializes a hash table with the attributes present in alist. *)
let rec hashtable_init (tbl : (attr, aval list) Hashtbl.t) (alist : attr list) : unit =
    match alist with
    | [] -> ()
    | atb :: atbs -> Hashtbl.add tbl atb [];
                    hashtable_init tbl atbs


(* Creates a new dataset with atts - the given attribute list. *)
let new_dataset (atts : attr list) : t =
    let tbl = Hashtbl.create 100 in
    let () = hashtable_init tbl atts in
    { n_attrs = List.length atts; attrs = atts; data = tbl; vals = [] }


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
            let atts = extract_attributes header in
            let ds = new_dataset atts in
            let () = process_rows tail ds in
            let _ = List.map (fun atb -> reverse_list ds atb) atts in
            ds

