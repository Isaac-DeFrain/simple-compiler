(* Lexer *)

(* defines [next_line] utility function;
   SyntaxError exception;
   sets up environment
*)
{
  open Lexing
  open Parser

  exception SyntaxError of string

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
                 pos_lnum = pos.pos_lnum + 1
      }
}

(* convenient REs *)
let digit = ['0'-'9']
let var = ['a'-'e']

let white = [' ' '\t']
let newline = '\r' | '\n' | "\r\n"

(* lexing rules *)
rule read =
  parse
  | white   { read lexbuf }
  | newline { next_line lexbuf; read lexbuf }
  | digit   { DIGIT (int_of_string (Lexing.lexeme lexbuf)) }
  | var     { VAR (Lexing.lexeme lexbuf) }
  | '='     { ASSIGN }
  | '?'     { READ }
  | '#'     { PRINT }
  | '+'     { ADD }
  | '-'     { SUB }
  | '*'     { MUL }
  | '&'     { AND }
  | "/\\"   { XOR }
  | ';'     { SEMICOLON }
  | '.'     { END }
  | eof     { EOF }
  | _       { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
