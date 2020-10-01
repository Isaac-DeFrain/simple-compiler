open TinyL

module Env = Map.Make(String)

type value =
  | Int of int
  | Undefined

type action = unit

type map = value Env.t

type global =
  { mutable env : map
  ; mutable read : map
  ; mutable print : action list
  }

let global =
  { env = Env.empty
  ; read = Env.empty
  ; print = []
  }

let id x = x

let print_value = function
  | Int i -> string_of_int i |> print_endline
  | Undefined -> print_endline "undefined"

let rec evalExpr = function
  | Add (e1, e2) ->
    (match (evalExpr e1, evalExpr e2) with
      Int v1, Int v2 -> Int (v1 + v2)
    | Undefined, _ -> Undefined
    | _, Undefined -> Undefined)
  | Sub (e1, e2) -> 
    (match (evalExpr e1, evalExpr e2) with
      Int v1, Int v2 -> Int (v1 - v2)
    | Undefined, _ -> Undefined
    | _, Undefined -> Undefined)
  | Mult (e1, e2) ->
    (match (evalExpr e1, evalExpr e2) with
      Int v1, Int v2 -> Int (v1 * v2)
    | Undefined, _ -> Undefined
    | _, Undefined -> Undefined)
  | And (e1, e2) ->
    (match (evalExpr e1, evalExpr e2) with
      Int v1, Int v2 -> Int (Int.logand v1 v2)
    | Undefined, _ -> Undefined
    | _, Undefined -> Undefined)
  | Xor (e1, e2) ->
    (match (evalExpr e1, evalExpr e2) with
      Int v1, Int v2 -> Int (Int.logxor v1 v2)
    | Undefined, _ -> Undefined
    | _, Undefined -> Undefined)
  | Var v -> Env.find v global.env
  | Dig d -> Int d

let evalStmt = function
  | Assign (v, exp) ->
    global.env <-
      let res = evalExpr exp in
      Env.add v res global.env
  | Read s ->
    let v = Int (int_of_string (read_line (print_string (s ^ " -> ")))) in
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
