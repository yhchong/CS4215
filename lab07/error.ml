(*
  Created 22-Feb-2006

  For error handling
*)

open Globals

type error = {
  error_loc : loc;
  error_text : string
}

(*exception Theorem_prover of (string * string) *)
exception Ppf of (error * int) (*Proving_pre_fails*)
(*
let all_errors : error list ref = ref []

let add_error e = all_errors := e :: !all_errors
*)

(* let report_error e = *)
(*   (match !proving_loc with *)
(*     | Some p -> *)
(*           Printf.printf "\nLast Proving Location: File \"%s\", line %d, col %d " *)
(*                p.start_pos.Lexing.pos_fname *)
(*                p.start_pos.Lexing.pos_lnum *)
(*               (p.start_pos.Lexing.pos_cnum - p.start_pos.Lexing.pos_bol) *)
(*     | None -> ()); *)
(*   Printf.printf "\nERROR: File \"%s\", line %d, col %d : %s \n" *)
(*       e.error_loc.start_pos.Lexing.pos_fname *)
(*       e.error_loc.start_pos.Lexing.pos_lnum *)
(*       (e.error_loc.start_pos.Lexing.pos_cnum - e.error_loc.start_pos.Lexing.pos_bol) *)
(*       e.error_text; *)
(*   flush stdout; *)
(*   failwith e.error_text *)

(* report error and don't care about the position *)
let report_error_msg (error_msg: string) =
  print_endline_q ("\nERROR MESSAGE: " ^ error_msg);
  (* flush stdout; *)
  failwith error_msg

let report_error e =
  print_endline_q ("\nERROR: at " ^ (string_of_loc e.error_loc)
                   ^ "\nMessage: " ^  e.error_text);
  (* flush stdout; *)
  failwith e.error_text

let report_no_pattern () = report_error {error_loc=no_pos; error_text="HIP/SLEEK error, unhandled pattern"}
(*asankhs: Lets not use such wording in external errors and exceptions - very poor coding, lazy programmers !!!*)

let report_error1 e s=
  print_endline_q e.error_text;
  (* flush stdout; *)
  failwith s

let report_warning e =
  if (!suppress_warning_msg) then ()
  else if (not !en_warning_msg) then report_error1 e "Warning->ERROR"
  else (
    print_endline_q ("\nWARNING: "
                     ^  (string_of_loc e.error_loc) ^ ":"
                     ^ e.error_text);
    (* flush stdout; *)
  )

exception Malformed_barrier of string
(*
let process_exct e=
  begin
      (match !proving_loc with
        | Some p ->
            Printf.printf "\nLast Proving Location: File \"%s\", line %d, col %d "
                p.start_pos.Lexing.pos_fname
                p.start_pos.Lexing.pos_lnum
                (p.start_pos.Lexing.pos_cnum - p.start_pos.Lexing.pos_bol)
        | None -> ());
      (match e with
        | Theorem_prover (prover_name, msg) ->
            Printf.printf "\nException:\"%s\",\n message: \"%s\" \n"
                ("theorem prover: " ^ prover_name) msg
        | _ -> print_endline (Printexc.to_string e)
      );
      dummy_exception() ;
  end
*)
