(* #load "dynlink.cma" *)
(* camlp4 -parser ex2_code_gen.cmo *)

open Camlp4.PreCast

type vec =
    | Scalar of string
    | Vector of string list
    | Sum of vec * vec
    | ScalarProduct of vec * vec

let expression = Gram.Entry.mk "expression"

  EXTEND Gram
  GLOBAL: expression ;
  expression:
      [ "sum" LEFTA
          [ x = SELF; "+"; y = SELF -> Sum (x, y) ]
      | "scalar" LEFTA
          [ x = SELF; "*"; y = SELF -> ScalarProduct (x, y) ]
      | "simple" NONA
          [ "("; e = SELF; ")" -> e
          | s = scalar -> Scalar s
          | v = vector -> v ] ];
  scalar:
      [ [ `INT (i, _) -> string_of_float (float i)
        | `FLOAT (_, f) -> f ] ]; (* this converts float to string *)
  vector:
      [ [ "["; v = LIST1 [ s = scalar -> s ] SEP ","; "]" -> Vector v ] ];
  END


let _loc = Loc.mk "<string>"


let rec generate_code = function
  | Scalar s -> <:expr< $flo:s$ >>
  | Vector vlist ->
      (* change this code to use only List.fold_right operation *)
      List.fold_right (fun x l -> <:expr< $flo:x$ :: $l$ >>) vlist <:expr< [] >>
  | Sum (v1, v2) ->
         let ast_v1 = generate_code  v1 in
         let ast_v2 = generate_code  v2 in
	(match (v1, v2) with
	  | Scalar _, Scalar _ 	-> <:expr< ( +. ) $ast_v1$ $ast_v2$ >>
	  |  _, _ -> <:expr< List.map2 ( +. ) $ast_v1$ $ast_v2$ >>)
  | ScalarProduct (v1, v2) ->
         let ast_v1 = generate_code v1
         and ast_v2 = generate_code v2
	in
	(match (v1, v2) with
	  | Scalar _, Scalar _ -> <:expr< ( *. ) $ast_v1$ $ast_v2$ >>
	  | Scalar _, _        -> <:expr< List.map ( fun a -> a *. $ast_v1$ ) $ast_v2$  >>
	  | _, Scalar _	       -> <:expr< List.map ( fun a -> a *. $ast_v2$ ) $ast_v1$ >>
	  | _, _	       -> <:expr< List.fold_right ( +. ) (List.map2 ( *. ) $ast_v1$ $ast_v2$ ) 0. >>)

let parse_and_generate_code str =
  let e = Gram.parse_string expression _loc str in
  generate_code e

let main =
  print_string "# ";
  let str = read_line () in
  let e = parse_and_generate_code str in
  let ast_e = <:expr< $e$ >> in
  Camlp4.PreCast.Printers.OCaml.print_implem <:str_item< let res = $ast_e$ in print_float res >>


(* expected testing:
 camlp4 -parser ex2_code_gen.cmo
 # 1+2
 let res = 1. +. 2. in print_float res;;

 camlp4 -parser ex2_code_gen.cmo
 # [1,2]+[3,4]
 let res = List.map2 ( +. ) [ 1.; 2. ] [ 3.; 4. ] in print_float res;;

 camlp4 -parser ex2_code_gen.cmo
 # [1,2]*3
 let res = List.map (fun a -> a *. 3.) [ 1.; 2. ] in print_float res;;

 camlp4 -parser ex2_code_gen.cmo
 # [1,2]*[3,4]
 let res =
    List.fold_right ( +. ) (List.map2 ( *. ) [ 1.; 2. ] [ 3.; 4. ]) 0.
  in print_float res;;
*)


