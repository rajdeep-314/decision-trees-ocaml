(*
    main.ml

    [ Give a description of what it does - an overview. ]

    Currently, this is just a little demo.
*)

open Dtree

(* Modules. *)
module Ds = Dataset
module T = Tree
module Q = Query

(* Read an in_channel and return a string. *)
let read_lines chan =
    let lines = ref [] in
    let () =
    try
        while true; do
            lines := input_line chan :: !lines
        done
    with End_of_file ->
        close_in chan
    in String.concat "\n" (List.rev !lines)


let file_name = List.nth (Array.to_list Sys.argv) 1

let lines = String.split_on_char '\n' (read_lines (open_in file_name))

let parsed_lines = Parser.parse_lines lines
let dset = Dataset.of_csv parsed_lines
let atb = List.nth dset.attrs 0

let () = Dataset.print dset
let () = Printf.printf "\n[ Removing the attribute ";
        Dataset.print_attr atb " now. ]\n\n"

let ndset = Dataset.rem_attr dset atb

let () = Dataset.print ndset

let ind = if 5 <= Dataset.size ndset then 4 else 0
let () = Printf.printf "\nThe label value for row %d is " (ind + 1)
let () = Dataset.print_lval (Dataset.nth_lval ndset ind) "\n"

let () = Printf.printf "Gain for the new first attribute = %f\n" (Heur.gain ndset (List.nth ndset.attrs 0))


(* Tree demonstration. *)

let tree : T.t = T.id3 dset

let () = Printf.printf "\n\nPrinting the decision tree now.\n\n"
let () = T.print_p tree


(* Query demonstration. *)

let () = Printf.printf "\n\nThe accuracy of the tree on the dataset is: %f percent\n" (100. *. (T.test_on_ds tree dset))

