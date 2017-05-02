
(* calling sPL parser *)
let parse_file (filename:string) : (string * SPL.sPL_expr) =
    SPL_parser.parse_file filename

let parse option_flag = 
  (* Read the arguments of command *)
  (* let _ = Arg.parse option_flag (fun s -> Globals.file := s) Globals.usage in *)
  (* let _ = Debug.read_main () in *)
  (* () *)
  let () = Debug.parse option_flag in
  ()

