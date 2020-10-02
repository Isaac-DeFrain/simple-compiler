(* RISC instructions *)

type register = Register.t

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
  
let string_of_reg (R i : register) = "R" ^ string_of_int i

let print_risc = function
  | LOADI (r, j) -> print_endline ("LOADI " ^ string_of_reg r ^ " #" ^ string_of_int j)
  | LOAD  (r, s) -> print_endline ("LOAD "  ^ string_of_reg r ^ " " ^ s)
  | STORE (s, r) -> print_endline ("STORE "  ^ s ^ " " ^ string_of_reg r)
  | ADD (r1, r2, r3) -> print_endline ("ADD " ^ string_of_reg r1 ^ " " ^ string_of_reg r2 ^ " " ^ string_of_reg r3)
  | SUB (r1, r2, r3) -> print_endline ("SUB " ^ string_of_reg r1 ^ " " ^ string_of_reg r2 ^ " " ^ string_of_reg r3)
  | MUL (r1, r2, r3) -> print_endline ("MUL " ^ string_of_reg r1 ^ " " ^ string_of_reg r2 ^ " " ^ string_of_reg r3)
  | AND (r1, r2, r3) -> print_endline ("AND " ^ string_of_reg r1 ^ " " ^ string_of_reg r2 ^ " " ^ string_of_reg r3)
  | XOR (r1, r2, r3) -> print_endline ("XOR " ^ string_of_reg r1 ^ " " ^ string_of_reg r2 ^ " " ^ string_of_reg r3)
  | READ s -> print_endline ("READ " ^ s)
  | WRITE s -> print_endline ("WRITE " ^ s)
