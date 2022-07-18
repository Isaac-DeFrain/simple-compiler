open TinyL
open Ast
open Lexing
open Parsing.Lexer
open Parsing.Parser
module ResMap = Map.Make (String)
open Core
open Poly

let parse_with_error lexbuf =
  let print_position outx lexbuf =
    let pos = lexbuf.lex_curr_p in
    fprintf
      outx
      "%s:%d:%d"
      pos.pos_fname
      pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol + 1)
  in
  try prog read lexbuf with
  | SyntaxError msg ->
      fprintf stderr "%a: %s\n" print_position lexbuf msg ;
      None
  | Error ->
      fprintf stderr "%a: syntax error\n" print_position lexbuf ;
      exit (-1)

let find_tests () =
  Sys_unix.getcwd () |> Sys_unix.readdir |> Array.to_list
  |> List.filter ~f:(fun s -> Str.string_match (Str.regexp ".*\\.tyl") s 0)
  |> List.sort ~compare

let res_map =
  let open Stdlib in
  let res1 =
    Some (Read "a", [Read "b"; Assign ("c", Add (Var "a", Var "b")); Print "c"])
  in
  let res2 =
    Some
      ( Read "a",
        [ Read "b";
          Assign ("c", And (Dig 3, Mul (Var "a", Var "b")));
          Assign ("d", Add (Var "c", Dig 1));
          Print "d" ] )
  in
  let res3 =
    Some
      ( Assign ("a", Add (Dig 1, Dig 2)),
        [ Assign
            ("b", Sub (Mul (Add (Dig 1, Add (Dig 2, Var "a")), Dig 5), Dig 8));
          Print "b" ] )
  in
  let res4 =
    Some
      ( Assign ("b", Add (Dig 5, Dig 8)),
        [ Assign ("a", Dig 1);
          Assign ("c", And (Var "a", Var "b"));
          Print "b";
          Print "a";
          Print "c" ] )
  in
  let res5 =
    Some
      ( Assign ("a", Add (Dig 0, Dig 1)),
        [Assign ("b", Add (Dig 1, Xor (Var "a", Dig 2))); Print "a"; Print "b"]
      )
  in
  let res6 =
    Some
      ( Read "a",
        [ Assign ("b", Add (Dig 1, Dig 2));
          Assign ("c", Add (Dig 1, Var "a"));
          Print "b" ] )
  in
  ResMap.empty
  |> ResMap.add "test1.tyl" res1
  |> ResMap.add "test2.tyl" res2
  |> ResMap.add "test3.tyl" res3
  |> ResMap.add "test4.tyl" res4
  |> ResMap.add "test5.tyl" res5
  |> ResMap.add "test6.tyl" res6

let%test _ =
  (let open In_channel in
  let res_map' = ref ResMap.empty in
  List.iter
    ~f:(fun filename ->
      let inx = create filename in
      let lexbuf = Lexing.from_channel inx in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename } ;
      res_map' := ResMap.add filename (parse_with_error lexbuf) !res_map' ;
      close inx)
    (find_tests ()) ;
  !res_map')
  = res_map
