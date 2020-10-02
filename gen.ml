(* RISC code generation *)

open TinyL
open Risc

let counter = ref 1

let get_counter () =
  let curr = !counter in
  counter := curr + 1; curr

module StrMap = Map.Make(String)

type location_map = register StrMap.t

let translateExpr = function
  | Add (e1, e2) ->
    begin
      match e1, e2 with
      | _ -> []
    end
  | Sub (e1, e2) ->
    begin
      match e1, e2 with
      | _ -> []
    end
  | Mul (e1, e2) ->
    begin
      match e1, e2 with
      | _ -> []
    end
  | And (e1, e2) ->
    begin
      match e1, e2 with
      | _ -> []
    end
  | Xor (e1, e2) ->
    begin
      match e1, e2 with
      | _ -> []
    end
  | Var s -> [LOAD (R (get_counter ()), s)]
  | Dig i -> [LOADI (R 0, i)]

let translateStmt = function
  | Assign (s, e) ->
    List.append (translateExpr e) [STORE (s, R 0)]
  | Read s -> [READ s]
  | Print s -> [WRITE s]
