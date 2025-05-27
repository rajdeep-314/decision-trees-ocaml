(*
    dtree.ml

    Decision tree type and functions for the same
*)


(*  'a  :   type of the attributes
            (the values of this type are our attributes)
    'l  :   type of the labels      *)
type ('a, 'l) dtree =
    | Leaf of 'l
    | Dtree of 'a * ('a, 'l) dtree * ('a, 'l) dtree


