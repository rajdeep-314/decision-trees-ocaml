(*
    main.ml

    [ Give a description of what it does - an overview. ]

    Currently, all this does is take a file name as input and create a dataset
    value out of it (with the type Dtree.Dataset.t). It then prints this dataset.
*)

open Dtree

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

let () = Dataset.print dset

