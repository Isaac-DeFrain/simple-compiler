open Ast
module Env = Map.Make (String)

type action = unit

type map = int Env.t

type global = { mutable env : map; mutable print : action list }

let global = { env = Env.empty; print = [] }

let id = Fun.id

let print_value i = string_of_int i |> print_endline

let rec eval_expr = function
  | Add (e1, e2) -> eval_expr e1 + eval_expr e2
  | Sub (e1, e2) -> eval_expr e1 - eval_expr e2
  | Mul (e1, e2) -> eval_expr e1 * eval_expr e2
  | And (e1, e2) -> Int.logand (eval_expr e1) (eval_expr e2)
  | Xor (e1, e2) -> Int.logxor (eval_expr e1) (eval_expr e2)
  | Var v -> Env.find v global.env
  | Dig d -> d

let eval_stmt = function
  | Assign (v, exp) ->
      global.env <-
        (let res = eval_expr exp in
         Env.add v res global.env)
  | Read s ->
      let v =
        print_string (s ^ " -> ")
        |> read_line
        |> int_of_string
      in
      global.env <- Env.add s v global.env
  | Print s ->
      global.print <-
        global.print
        @ [ print_value (Env.find s global.env)
          ; print_string (s ^ " -> ")
          ]

let rec eval_stmts = function
  | [] -> List.iter id global.print
  | hd :: tl ->
      global.print <- global.print @ [eval_stmt hd] ;
      eval_stmts tl

let eval (s0, ss) = eval_stmts (s0 :: ss)

(* unit tests *)
let%test _ =
  let d = Random.int 10 in
  eval_expr (Dig d) = d
