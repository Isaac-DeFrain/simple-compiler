(* Optimize RSIC code *)

open Risc

module VarMap = Map.Make(String)

type store = register VarMap.t 

(* deduplicate list elements *)
let rec dedup = function
  | [] -> []
  | hd :: tl ->
    hd :: dedup (List.filter (fun x -> x != hd) tl)

(* Registers used in a given RISC instruction *)
let registers_used_in_instruction = function
  | LOADI (r, _) -> [r]
  | LOAD  (r, _) -> [r]
  | STORE (_, r) -> [r]
  | ADD (r0, r1, r2) -> [r0; r1; r2]
  | SUB (r0, r1, r2) -> [r0; r1; r2]
  | MUL (r0, r1, r2) -> [r0; r1; r2]
  | AND (r0, r1, r2) -> [r0; r1; r2]
  | XOR (r0, r1, r2) -> [r0; r1; r2]
  | _ -> []

(* Registers used in RISC instruction list *)
let rec registers = function
  | [] -> []
  | hd :: tl ->
    registers_used_in_instruction hd @ registers tl
    |> dedup
    |> List.sort Register.compare

let is_read = function
  | READ _ -> true
  | _ -> false

(* Maps variables to registers *)
let store_reg s = function
  | STORE (v, r) ->
    VarMap.add v r s
  | _ -> s

let rec store_regs s = function
  | [] -> s
  | hd :: tl ->
    store_regs (store_reg s hd) tl

(* Generates the store map from an instruction list *)
let store = store_regs VarMap.empty
    
(* Determines the registers contributing to output *)
let write_register s = function
  | WRITE v -> [VarMap.find v s]
  | _ -> []

let rec write_registers s = function
  | [] -> []
  | hd :: tl ->
    write_register s hd @ write_registers s tl
    |> List.sort Register.compare

let write_regs ilist = write_registers (store ilist) ilist

(* Determines list of registers contributing to the given register *)
let used r = function
  | ADD (r0, r1, r2) -> if r = r0 || r = r1 || r = r2 then [r0; r1; r2] else []
  | SUB (r0, r1, r2) -> if r = r0 || r = r1 || r = r2 then [r0; r1; r2] else []
  | MUL (r0, r1, r2) -> if r = r0 || r = r1 || r = r2 then [r0; r1; r2] else []
  | AND (r0, r1, r2) -> if r = r0 || r = r1 || r = r2 then [r0; r1; r2] else []
  | XOR (r0, r1, r2) -> if r = r0 || r = r1 || r = r2 then [r0; r1; r2] else []
  | _ -> []

(* Determines list of registers contributing to
   a given register in a given instruction list *)
let used_regs r ilist =
  List.map (used r) ilist
  |> List.concat
  |> dedup

(* Determines list of registers contributing to a given
   register in a given instruction list *)
let contribute l r = used_regs r l

(* Determines the list of registers that contribute to output *)
let contributing_regs ilist =
  let w = write_regs ilist in
  let rec contributing_regs' i l =
    let l' =
      List.map (contribute i) l
      |> List.concat
      |> dedup
      |> List.sort Register.compare
    in
    if l = l' then l
    else contributing_regs' i l'
  in
  contributing_regs' ilist w
  |> dedup
  |> List.sort Register.compare

(* Eliminates RISC code not involved in I/O *)
let optimize ilist =
  let c = contributing_regs ilist in
  let necessary =
    fun i ->
      List.exists
        (fun r -> List.mem r c)
        (registers_used_in_instruction i)
      || is_read i
  in
  List.filter necessary ilist
