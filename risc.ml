(* RISC instructions *)

type register = R of int

type risc_instruction =
  (* memory *)
  | LOADI of register * int
  | LOAD  of register * string
  | STORE of string * register
  (* arithmetic *)
  | ADD of register * register * register
  | SUB of register * register * register
  | MUL of register * register * register
  | AND of register * register * register
  | XOR of register * register * register
  (* i/o *)
  | READ  of string
  | WRITE of string
