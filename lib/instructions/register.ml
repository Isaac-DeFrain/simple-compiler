(* Register module *)

type t = R of int

let compare (R i) (R j) = Int.compare i j
