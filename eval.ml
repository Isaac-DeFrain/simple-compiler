open TinyL

module Env = Map.Make(String)

type action = unit

type map = int Env.t

type global =
  { mutable env : map
  ; mutable print : action list
  }

let global =
  { env = Env.empty
  ; print = []
  }

let id x = x

let print_value i = string_of_int i |> print_endline

let rec evalExpr = function
  | Add (e1, e2) -> evalExpr e1 + evalExpr e2
  | Sub (e1, e2) -> evalExpr e1 - evalExpr e2
  | Mul (e1, e2) -> evalExpr e1 * evalExpr e2
  | And (e1, e2) -> Int.logand (evalExpr e1) (evalExpr e2)
  | Xor (e1, e2) -> Int.logor (evalExpr e1) (evalExpr e2)
  | Var v -> Env.find v global.env
  | Dig d -> d

let evalStmt = function
  | Assign (v, exp) ->
    global.env <-
      let res = evalExpr exp in
      Env.add v res global.env
  | Read s ->
    let v = int_of_string (read_line (print_string (s ^ " -> "))) in
      global.env <- Env.add s v global.env
  | Print s ->
    global.print <- 
      List.append global.print
        [ print_value (Env.find s global.env)
        ; print_string (s ^ " -> ")]

let rec evalStmts = function
  | [] -> List.iter id global.print
  | hd :: tl ->
    global.print <- List.append global.print [evalStmt hd];
    evalStmts tl

let eval (s0, ss) = evalStmts (s0 :: ss)
