open SPLc
open SVM

(* translating command *)
let trans_cmd op =
  match op with
    | "+" -> PLUS
    | "-" -> MINUS
    | "*" -> TIMES
    | "/" -> DIV
    | "\\" -> NOT
    | "~" -> NEG
    | "|" -> OR
    | "&" -> AND
    | "<" -> LT
    | ">" -> GT
    | "=" -> EQ
    | _ -> failwith ("no such operator "^op)

open Debug
open Gen
open SPL_type
module S = SPL

let labels = new generator "label"

type c_env = int Environ.et

let enum_cenv xs n =
  let rec aux xs n =
    match xs with
      | [] -> []
      | x::xs -> (x,n)::(aux xs (n+1))
  in aux xs n

let rec get_join_type v u = match v, u with
    | v, (u::us) ->
            let res = get_join_type v us in
            if List.mem u v
            then res
            else u::res
    | _, [] -> []


let compare_rows t1 t2 r1 r2 t =
    let rec get_tag t1 row tag =
        match t1, row with
        | t::ts, v::vs ->
                if tag=t then (true, v)
                else get_tag ts vs tag
        | [], [] -> (false, IntConst 42) in (*42 dummy value*)
    let rec matching r1 r2 ts =
        match ts with
        | t::ts ->
            let succ1, d1 = get_tag t1 r1 t in
            let succ2, d2 = get_tag t2 r2 t in
            if succ1 && succ2
            then d1=d2 && matching r1 r2 ts
            else matching r1 r2 ts
        | [] -> true in
    let rec get_unique t2 r2 =
        match t2, r2 with
        | t::ts, r::rs ->
            if List.mem t t1
            then get_unique ts rs
            else r::(get_unique ts rs)
        | [], [] -> [] in
    Printf.printf "\nComparing rows\n";
    if matching r1 r2 t
    then (true, r1@(get_unique t2 r2))
    else (false, [])

let rec check_row t1 t2 r rs t =
    match rs with
    | x::xs ->
            let succ, data = compare_rows t1 t2 r x t in
            if succ then data::(check_row t1 t2 r xs t)
            else check_row t1 t2 r xs t
    | [] -> []

let rec unique exps =
    match exps with
    | e::es ->
            if List.mem e es
            then unique es
            else e::(unique es)
    | [] -> []

let perform_join t1 t2 exps1 exps2 =
    let rec helper t1 t2 exps1 exps2 t =
        (match exps1 with
        | v::vs -> (check_row t1 t2 v exps2 t)@(helper t1 t2 vs exps2 t)
        | _ -> []) in
    let t = t1@(get_join_type t1 t2) in
    let rows = helper t1 t2 exps1 exps2 t in
    let rows = unique rows in
    Rel(RelType(t), rows)

let rec perform_proj t exps ids =
    let rec mem t ids =
        match t with
        | (id, _) -> List.mem id ids in
    let rec get_rel_type v u = match v with
        | v::vs ->
                if mem v u
                then v::(get_rel_type vs u)
                else get_rel_type vs u
        | [] -> [] in
    let rec check_row t row =
        match t, row with
        | t::ts, elem::rest->
                if mem t ids
                then elem::(check_row ts rest)
                else check_row ts rest
        | [], [] -> [] in
    let rec helper t exps =
        match exps with
        | exp::exps -> (check_row t exp)::(helper t exps)
        | [] -> [] in
    let t1 = get_rel_type t ids in
    let exps = helper t exps in
    let exps = unique exps in
    Rel(RelType(t1), exps)

let rec run (e:sPL_expr) =
    match e with
    | BinaryPrimApp (op, e1, e2) ->
            (match e1, e2 with
                | Rel(RelType(t1), exps1), Rel(RelType(t2), exps2) ->
                        if op="@"
                        then perform_join t1 t2 exps1 exps2
                        else failwith "Unsupported op"
                | _, _ -> failwith "Unsupported type")
    | Proj (e, ids) ->
            match e with
            | Rel(RelType (t1), exps) ->
                    perform_proj t1 exps ids
    | _ -> failwith "Unsupported run"

(* compiling to eVML instrs *)
let compile (e:sPL_expr) : sVML_prog_sym   =
  let rec helper (ce:c_env) (e:sPL_expr) : sVML_prog_sym * sVML_prog_sym  =
    match e with
      | IntConst i -> [LDCI i],[]
      | BoolConst b -> [LDCB b],[]
      | UnaryPrimApp (op,arg) ->
            let (s,p) = helper ce arg in
            (s@[trans_cmd op],p)
      | BinaryPrimApp (op,arg1,arg2) ->
            let (s1,p1) = helper ce arg1 in
            let (s2,p2) = helper ce arg2 in
            (s1@(s2@[trans_cmd op]),p1@p2)
      | Var s ->
            begin
              match Environ.get_val ce s with
                | Some i -> [LD (s,i)],[]
                | None -> [LD (s,(-1))],[]
            end
      | Func (t,vs,body) ->
            let l_fn = labels # fresh_id in
            let fvs = diff (fv body) vs in
            let all_vs = fvs@vs in
            let new_ce = enum_cenv all_vs 0 in
            let arity = List.length vs in
            let (s1,p1) = helper new_ce body in
            let fvs_n = List.map
              (fun v -> match (Environ.get_val ce v) with
                | Some i ->(v,i)
                | _ -> (v,-1)) fvs in
            ([LDF (fvs_n,arity,l_fn)], (((LABEL l_fn)::s1)@[RTN]@p1))
      | Cond (e1,e2,e3) ->
            let l1 = labels # fresh_id in
            let l2 = labels # fresh_id in
            let (s1, p1) = helper ce e1 in
            let (s2, p2) = helper ce e2 in
            let (s3, p3) = helper ce e3 in
            (s1@[JOF l1]@s2@[GOTO l2]@[LABEL l1]@s3@[LABEL l2], p1@p2@p3)
      | Appln (f,_,args) ->
              let rec convert_s seq =
                  match seq with
                  | (s, p)::xs -> (convert_s xs)@s
                  | [] -> [] in
              let rec convert_p seq =
                  match seq with
                  | (s, p)::xs -> (convert_p xs)@p
                  | [] -> [] in
              let (fs, fp) = helper ce f in
              let arg_val = List.map (fun arg -> helper ce arg) args in
              let s = convert_s arg_val in
              let p = convert_p arg_val in
              let arity = List.length args in
              (s@fs@[CALL arity], fp@p)
      | RecFunc (t,f,vs,body) ->
              let l_fn = labels # fresh_id in
              let fvs = diff (fv body) vs in
              let all_vs = fvs@vs in
              let new_ce = enum_cenv all_vs 0 in
              let arity = List.length vs in
              let (s1, p1) = helper new_ce body in
              let fvs_n = List.map
                (fun v -> match (Environ.get_val ce v) with
                  | Some i ->Printf.printf "%s:%i " v i;(v,i)
                  | _ -> (v,-1)) fvs in
              let fvs_n = List.filter (fun (v, _) -> v <> f) fvs_n in
              ([LDFR (fvs_n, (f, 0), arity,l_fn)], [LABEL l_fn]@s1@[RTN]@p1) 
  in
  let (main_code,proc_code) = (helper [] e)
  in main_code@(DONE::proc_code)

let collect_label xs =
  let rec aux xs n =
    match xs with
      | [] -> []
      | x::xs ->
            begin
            match x with
              | LABEL s -> (s,n)::(aux xs n)
              | _ -> aux xs (n+1)
            end
  in aux xs 0

let trans env c =
  let get_addr l =
    match (Environ.get_val env l) with
      | Some a -> a
      | None -> failwith ("linking problem : unrecognised label :"^l)
  in
  match c with
    | GOTO l -> GOTO (get_addr l)
    | JOF l -> JOF (get_addr l)
    | LABEL l -> LABEL (get_addr l)
    | LDF (g,a,l) -> LDF (g,a,get_addr l)
    | LDFR (g,f,a,l) -> LDFR (g,f,a,get_addr l)
    | LDCI i -> LDCI i
    | LDCB b -> LDCB b
    | PLUS -> PLUS
    | MINUS -> MINUS
    | TIMES -> TIMES
    | DIV -> DIV
    | AND -> AND
    | NEG -> NEG
    | NOT -> NOT
    | NOP -> NOP
    | OR -> OR
    | LT -> LT
    | GT  -> GT
    | EQ  -> EQ
    | DONE -> DONE
    | LD i -> LD i
    | CALL n -> CALL n
    | TAILCALL n -> TAILCALL n
    | RTN -> RTN

let tail_optimize (xs:sVML_prog_sym) : sVML_prog_sym =
  (* to perform tail-call optimization *)
  failwith ("TAIL OPTIMISE - TO BE IMPLEMENTED")

let link_code (xs:sVML_prog_sym) : sVML_prog_mc =
  (* to convert labels to addresses *)
  let env = collect_label xs in
      (List.map (trans env) xs)

let filter_label (xs:sVML_prog_mc) : sVML_prog_mc =
  List.filter (fun c -> match c with LABEL _ -> false | _ -> true) xs


(* output evm bytecode to a file *)
let generate_bytecode(elist: sVML_inst_mc list) (filename:string) =
  let out_channel = open_out_bin filename in
  output_value out_channel (Array.of_list elist);
  close_out out_channel

let generate_bytecode(elist: sVML_inst_mc list) (filename:string) =
  let pr x = string_of_int (List.length x) in
  Debug.no_2 "generate_bytecode" pr pr_id pr_none generate_bytecode elist filename

let extract_filename (s:string) : string =
  let v = String.length s in
  if v<5 then
    failwith "filename at least one char"
  else
    let fn = String.sub s 0 (v-4) in
    let extn = String.sub s (v-4) 4 in
    if extn = ".spl" then fn
    else failwith "filename must have .spl extn"
;;

(* test driver for extract_argument *)
let test_extr_filename () =
  print_endline (extract_filename "hello.epl");
  (* should return "hello" *)
  print_endline (extract_filename ".epl");
  (* should return Failure("filename at least one char") *)
  print_endline (extract_filename "hello.ep")
  (* should return Failure("filename must have .epl extn") *)
;;

(* test_extr_filename ();; *)

(* let usage = "usage: " ^ Sys.argv.(0) ^ " [options] <filename>" *)

(* (\* calling sPL parser *\) *)
(* let parse_file (filename:string) : (string * S.sPL_expr) = *)
(*   SPL_parser.parse_file filename *)

(* main program *)
let main =
  (* Read the arguments of command *)
  let _ = Debug.parse option_flag in
  (* let _ = Debug.read_main () in *)
  if String.length !VarGen.file == 0 then print_endline VarGen.usage else
    let _ = print_endline "Loading sPL program .." in
    let (s,p) = Genparse.parse_file !VarGen.file in
    let _ = print_endline ("  "^s) in
    let _ = print_endline ("  as "^(S.string_of_sPL p)) in
    Printf.printf "Done\n";
    let _ = print_endline "TYPE CHECKING program .." in
    let (v,np) = type_infer [] p in
    match v with
      | None -> print_endline " ==> type error detected"
      | Some t ->
          print_endline (" ==> inferred type "^(S.string_of_sPL_type t));
            let _ = print_string "TRANSFORMING ==> " in
            let np = trans_exp np in
            let _ = print_endline (string_of_sPL np) in
            let fn = extract_filename !VarGen.file in
            if true then
                let _ = print_string "RUNNING ==> " in
                let np = run np in
                let _ = print_endline (string_of_sPL np) in
                print_endline "Done\n"
            else
              let _ = print_endline (string_of_sPL np) in
              Printf.printf "Done\n"
            (*let bytefn = fn^".svm" in
            let _ = print_string ("COMPILING ==> "^bytefn^"\n") in
            let r = compile np in
            let _ = print_endline (string_of_sVML_list_sym r) in
            let r =
              if !tail_optimize_flag then
                let r = tail_optimize r in
                let _ = print_endline ("TAIL-OPTIMIZE ==> ") in
                let _ = print_endline (string_of_sVML_list_sym r) in
                r
              else r in
            let _ = print_string ("LINKING ==> \n") in
            let s = link_code r in
            let _ = print_endline (string_of_sVML_list s) in
            let s = filter_label s in
            let _ = generate_bytecode s bytefn in
            ()*)
