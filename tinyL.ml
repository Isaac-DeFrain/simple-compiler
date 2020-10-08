(* tinyL AST *)
open Core

type 'a ne_list = 'a * 'a list
  [@@deriving sexp_of]

type pgm = stmt ne_list
  [@@deriving sexp_of]
  
and stmt =
  | Assign of string * expr
  | Read   of string
  | Print  of string

and expr =
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | And of expr * expr
  | Xor of expr * expr
  | Var of string
  | Dig of int

(* pretty printing *)
open Out_channel

let rec print_expr outc = function
  | Add (a, b) -> print_symbol_exps outc "ADD " a b
  | Sub (a, b) -> print_symbol_exps outc "SUB " a b
  | Mul (a, b) -> print_symbol_exps outc "MUL " a b
  | And (a, b) -> print_symbol_exps outc "AND " a b
  | Xor (a, b) -> print_symbol_exps outc "XOR " a b
  | Var v -> output_string outc (v ^ " ")
  | Dig d -> output_string outc (string_of_int d ^ " ")

and print_symbol_exps outc sym a b =
  output_string outc (sym);
  print_expr outc a;
  print_expr outc b

let print_assign outc = function
  | (v, e) ->
    output_string outc v;
    output_string outc " = ";
    print_expr outc e

let print_read outc = function
  | v ->
    output_string outc "READ ";
    output_string outc v

let print_print outc = function
  | v ->
    output_string outc "PRINT ";
    output_string outc v

let print_stmt outc = function
  | Assign (v, e) -> print_assign outc (v, e)
  | Read e  -> print_read outc e
  | Print e -> print_print outc e

let rec print_stmt_list outc = function
  | [] -> ()
  | hd :: tl ->
    print_stmt outc hd;
    output_string outc ";\n";
    print_stmt_list outc tl

(* print formatted AST *)
let print_tiny outc = function
  | (s, l) ->
    print_stmt outc s;
    output_string outc ";\n";
    print_stmt_list outc l

(* print sexp *)
let print_sexp outc p =
  let s = Sexp.to_string_hum (sexp_of_pgm p) in
  output_string outc s
