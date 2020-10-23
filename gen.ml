(* RISC code generation *)

open TinyL
open Risc
open Register

let counter = ref 1

let next_reg () =
  let curr = !counter in
  counter := curr + 1 ;
  R curr

let rec translateExpr r = function
  | Add (e1, e2) ->
      let r1 = next_reg () in
      let r2 = next_reg () in
      translateExpr r1 e1 @ translateExpr r2 e2 @ [ADD (r, r1, r2)]
  | Sub (e1, e2) ->
      let r1 = next_reg () in
      let r2 = next_reg () in
      translateExpr r1 e1 @ translateExpr r2 e2 @ [SUB (r, r1, r2)]
  | Mul (e1, e2) ->
      let r1 = next_reg () in
      let r2 = next_reg () in
      translateExpr r1 e1 @ translateExpr r2 e2 @ [MUL (r, r1, r2)]
  | And (e1, e2) ->
      let r1 = next_reg () in
      let r2 = next_reg () in
      translateExpr r1 e1 @ translateExpr r2 e2 @ [AND (r, r1, r2)]
  | Xor (e1, e2) ->
      let r1 = next_reg () in
      let r2 = next_reg () in
      translateExpr r1 e1 @ translateExpr r2 e2 @ [XOR (r, r1, r2)]
  | Var v -> [LOAD (r, v)]
  | Dig i -> [LOADI (r, i)]

let translateStmt = function
  | Assign (v, e) ->
      let r = next_reg () in
      translateExpr r e @ [STORE (v, r)]
  | Read v -> [READ v]
  | Print v -> [WRITE v]

let rec translateStmts = function
  | [] -> []
  | hd :: tl -> translateStmt hd @ translateStmts tl

let translate (s0, ss) = translateStmts (s0 :: ss)
