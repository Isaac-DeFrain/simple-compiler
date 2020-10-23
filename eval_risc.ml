(* Evaluation of RISC code *)

open Risc
module VarMap = Map.Make (String)
module RegMap = Map.Make (Register)

type value_map = int VarMap.t

type store_map = int RegMap.t

type env = value_map ref

type store = store_map ref

let env = ref VarMap.empty

let store = ref RegMap.empty

let eval_instruction = function
  | LOADI (r, i) -> store := RegMap.add r i !store
  | LOAD (r, s) ->
      let i = VarMap.find s !env in
      store := RegMap.add r i !store
  | STORE (s, r) ->
      let i = RegMap.find r !store in
      env := VarMap.add s i !env
  | ADD (r0, r1, r2) ->
      let i1 = RegMap.find r1 !store in
      let i2 = RegMap.find r2 !store in
      store := RegMap.add r0 (i1 + i2) !store
  | SUB (r0, r1, r2) ->
      let i1 = RegMap.find r1 !store in
      let i2 = RegMap.find r2 !store in
      store := RegMap.add r0 (i1 - i2) !store
  | MUL (r0, r1, r2) ->
      let i1 = RegMap.find r1 !store in
      let i2 = RegMap.find r2 !store in
      store := RegMap.add r0 (i1 * i2) !store
  | AND (r0, r1, r2) ->
      let i1 = RegMap.find r1 !store in
      let i2 = RegMap.find r2 !store in
      store := RegMap.add r0 (Int.logand i1 i2) !store
  | XOR (r0, r1, r2) ->
      let i1 = RegMap.find r1 !store in
      let i2 = RegMap.find r2 !store in
      store := RegMap.add r0 (Int.logxor i1 i2) !store
  | READ s ->
      print_string (s ^ " -> ") ;
      let i = read_int () in
      env := VarMap.add s i !env
  | WRITE s ->
      let i = VarMap.find s !env in
      print_endline (s ^ " -> " ^ string_of_int i)

let rec eval_risc = function
  | [] -> ()
  | hd :: tl ->
      eval_instruction hd ;
      eval_risc tl
