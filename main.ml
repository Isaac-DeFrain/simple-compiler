open Core
open Lexer
open Lexing
open Eval
open Risc
open Eval_risc

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf
    outx
    "%s:%d:%d"
    pos.pos_fname
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.prog read lexbuf with
  | SyntaxError msg ->
      fprintf stderr "%a: %s\n" print_position lexbuf msg ;
      None
  | Parser.Error ->
      fprintf stderr "%a: syntax error\n" print_position lexbuf ;
      exit (-1)

(* parse and print formatted code *)
let rec parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | Some value ->
      printf "%a\n" TinyL.print_tiny value ;
      parse_and_print lexbuf
  | None -> ()

(* parse and print sexp *)
let rec parse_and_print_sexp lexbuf =
  match parse_with_error lexbuf with
  | Some value ->
      printf "%a\n" TinyL.print_sexp value ;
      parse_and_print_sexp lexbuf
  | None -> ()

(* parse and evaluate *)
let parse_and_eval lexbuf =
  match parse_with_error lexbuf with Some pgm -> eval pgm | None -> ()

(* parse and translate *)
let parse_and_translate lexbuf =
  match parse_with_error lexbuf with
  | Some pgm -> Gen.translate pgm |> List.iter ~f:print_risc
  | None -> ()

(* parse, translate, and eval *)
let parse_translate_eval lexbuf =
  match parse_with_error lexbuf with
  | Some pgm -> Gen.translate pgm |> eval_risc
  | None -> ()

(* parse, translate, and optimize *)
let parse_and_optimize lexbuf =
  match parse_with_error lexbuf with
  | Some pgm ->
      Gen.translate pgm |> Optimize.optimize |> List.iter ~f:print_risc
  | None -> ()

(* Interactive mode *)
let interact e ast =
  let open In_channel in
  let pp =
    match (e, ast) with
    | (true, _) -> parse_and_eval
    | (_, a) -> if a then parse_and_print_sexp else parse_and_print
  in
  match input_line stdin with
  | None -> ()
  | Some code ->
      Out_channel.newline stdout ;
      let lexbuf = Lexing.from_string code in
      pp lexbuf ;
      close stdin

(* Read code from file *)
let from_file f ast =
  let open In_channel in
  let pp = if ast then parse_and_print_sexp else parse_and_print in
  match f with
  | None -> ()
  | Some filename ->
      let inx = create filename in
      let lexbuf = Lexing.from_channel inx in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename } ;
      pp lexbuf ;
      close inx

(* Eval mode *)
let eval_file f =
  let open In_channel in
  match f with
  | None -> ()
  | Some filename ->
      let inx = create filename in
      let lexbuf = Lexing.from_channel inx in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename } ;
      parse_and_eval lexbuf ;
      close inx

(* Translate to RISC *)
let translate f =
  let open In_channel in
  match f with
  | None -> ()
  | Some filename ->
      let inx = create filename in
      let lexbuf = Lexing.from_channel inx in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename } ;
      parse_and_translate lexbuf ;
      close inx

(* Translate to RISC and evaluate *)
let translate_and_eval f =
  let open In_channel in
  match f with
  | None -> ()
  | Some filename ->
      let inx = create filename in
      let lexbuf = Lexing.from_channel inx in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename } ;
      parse_translate_eval lexbuf ;
      close inx

(* Translate to RISC and optimize *)
let translate_optimize f =
  let open In_channel in
  match f with
  | None -> ()
  | Some filename ->
      let inx = create filename in
      let lexbuf = Lexing.from_channel inx in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename } ;
      parse_and_optimize lexbuf ;
      close inx

(* handler function *)
let handler f i ast e r o () =
  if r then
    if e then translate_and_eval f
    else if o then translate_optimize f
    else translate f
  else if i then interact e ast
  else if e then eval_file f
  else from_file f ast

(* cli *)
let () =
  Command.basic_spec
    ~summary:"Parse and display TinyL code"
    Command.Spec.(
      empty
      +> flag "-f" (optional string) ~doc:" Read code from file"
      +> flag "-i" no_arg ~doc:" Interpreter mode"
      +> flag "-ast" no_arg ~doc:" Output sexp AST"
      +> flag "-e" no_arg ~doc:" Evaluate"
      +> flag "-risc" no_arg ~doc:" Translate to RISC instructions"
      +> flag "-o" no_arg ~doc:" Optimize RISC code")
    handler
  |> Command.run
