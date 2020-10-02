(* RISC code generation *)

open TinyL
open Risc
open Register

let counter = ref 1

let next_reg () =
  let curr = !counter in
  counter := curr + 1; R curr

let rec translateExpr r = function
  | Add (e1, e2) ->
    let r1 = next_reg () in
    let r2 = next_reg () in
    List.append
      (List.append
        (translateExpr r1 e1)
        (translateExpr r2 e2))
      [ADD (r, r1, r2)]
  | Sub (e1, e2) ->
    let r1 = next_reg () in
    let r2 = next_reg () in
    List.append
      (List.append
        (translateExpr r1 e1)
        (translateExpr r2 e2))
      [SUB (r, r1, r2)]
  | Mul (e1, e2) ->
    let r1 = next_reg () in
    let r2 = next_reg () in
    List.append
      (List.append
        (translateExpr r1 e1)
        (translateExpr r2 e2))
      [MUL (r, r1, r2)]
  | And (e1, e2) ->
    let r1 = next_reg () in
    let r2 = next_reg () in
    List.append
      (List.append
        (translateExpr r1 e1)
        (translateExpr r2 e2))
      [AND (r, r1, r2)]
  | Xor (e1, e2) ->
    let r1 = next_reg () in
    let r2 = next_reg () in
    List.append
      (List.append
        (translateExpr r1 e1)
        (translateExpr r2 e2))
      [XOR (r, r1, r2)]
  | Var s -> [LOAD (r, s)]
  | Dig i -> [LOADI (r, i)]

let translateStmt = function
  | Assign (s, e) ->
    List.append (translateExpr (R 0) e) [STORE (s, R 0)]
  | Read s -> [READ s]
  | Print s -> [WRITE s]

let rec translateStmts = function
  | [] -> []
  | hd :: tl -> List.append (translateStmt hd) (translateStmts tl)

let translate (s0, ss) = translateStmts (s0 :: ss)
