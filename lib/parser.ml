(*
    CSV Parser:

        The main function that this file provides is to take a 
        CSV line and tokenize it to return a list of tokens, where
        each token is just the Tok constructor applied to a string.

        Another one is to take a bunch of CSV lines and tokenize them
        to return a list of lists of tokens - this is a simple List.map
        using the parse_line function.

    NOTE:   It is assumed that the text being parsed is in
    valid CSV format.
*)


(* A token is defined as Tok of a string. *)
type t = Tok of string


(* Characters to be ignored (for stripping). *)
let to_ignore : char list =
    [ ' '; '\n'; '\t'; '\r'; ',' ]

(* Characters to split based on. *)
let to_split : char list =
    [ ',' ]

(* If c is to be ignored or not. *)
let is_ignored (c : char) : bool = List.mem c to_ignore

(* If c is to be split based-on or not. *)
let is_split (c : char) : bool = List.mem c to_split

(* s[:1], in Python terms. *)
let head (s : string) : string =
    if s = "" then s
    else String.sub s 0 1

(* s[1:], in Python terms. *)
let tail (s : string) : string =
    if s = "" then s 
    else String.sub s 1 (String.length s - 1)


(* Strip any contiguous occurances of characters from to_ignore
   at the beginning of a string. *)
let rec strip_leading (s : string) : string =
    if s = "" then s
    else if is_ignored s.[0] then strip_leading (tail s)
    else s


(* Helper function for the tokenize function.
   s is the string currently being looked at, acc is the accumulated
   string. *)
let rec tokenize_unit_helper (s : string) (acc : string) : t * string =
    if s = "" then Tok acc, ""
    else if is_split s.[0] then Tok acc, strip_leading s
    else tokenize_unit_helper (tail s) (acc ^ (head s))

(* Performs one unit of tokenization on s. *)
let tokenize_unit (s : string) : t * string = tokenize_unit_helper s ""


(* Performs tokenization starting from s, with the accumulated tokens
   being in acc - a list of tokens. *)
let rec tokenize_helper (s : string) (acc : t list) : t list =
    if s = "" then acc
    else match tokenize_unit s with
        | (tok, str) -> tokenize_helper str (acc @ [tok])


(*
   Parse a line wth comma separated values and return the
   corresponding token list.

   Example:
       parse_line "abcd, efgh, x,   y z" ->
           [ Tok "abcd"; Tok "efgh"; Tok "x"; Tok "y z" ]
*)
let parse_line (s : string) : t list = tokenize_helper s []

(* Parse a list of CSV lines. *)
let parse_lines (l : string list) : t list list =
            List.map parse_line l

