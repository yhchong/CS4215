#include "xdebug.cppo"
(* PLEASE DO NOT CHANGE THIS FILE *)
open SPL_type
open Debug
open SPLc
module S = SPL

let parse_file (filename:string) : (string * SPL.sPL_expr) =
    SPL_parser.parse_file filename

(* main program *)
let main =
  if String.length !VarGen.file == 0 then print_endline VarGen.usage else 
    let _ = print_endline "LOADING sPL program .." in
    let (s,p) = parse_file !VarGen.file in
    let _ = print_endline ("  "^s) in
    let _ = print_endline (" AS ==> "^(S.string_of_sPL p)) in
    let _ = print_endline "TYPE CHECKING program .." in
    let (v,np) = type_infer [] p in
    match v with
      | None -> print_endline " ==> type error detected"
      | Some t ->
            begin
              print_endline (" ==> inferred type "^(S.string_of_sPL_type t));
              let _ = print_string "TRANSFORMING ==> " in
              let np = trans_exp np in
              let _ = print_endline (string_of_sPL np) in
              ()
            end
