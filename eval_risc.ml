(* Evaluation of RISC code *)

open Risc

module StrMap = Map.Make(String)
module RegMap = Map.Make(Register)

type location_map = register StrMap.t
type store_map = int RegMap.t

type store = store_map ref
