#include "xdebug.cppo"

open SPL
open Debug
(* open Gen *) 
module C = SPLc

type env_type = sPL_type Environ.et
let tail_optimize_flag = ref false
let pa_removal_flag = ref true
let stack_size = ref 10000

let option_flag = [
  ("--tail", Arg.Set tail_optimize_flag, "Enable tail-call optimization.")
  ;("--dis-pa", Arg.Clear pa_removal_flag, "Disable partial application Removal.")
  ;("-stk-size", Arg.Set_int stack_size,
    "Size of Stack Memory (default is 10000)")
  (* ;("-dre", Arg.String (fun s -> *)
  (*     DebugCore.z_debug_file:=("$"^s); DebugCore.z_debug_flag:=true), *)
  (*  "Shorthand for -debug-regexp") *)
]@command_args

let _ = Debug.parse option_flag

(* if (v,r) in env, return Some r *)
(* otherwise, return None *)
let get_type env v = Environ.get_val env v

(* match a function type t with its parameters args *)
(* and return its residual *)
(* extr_arg_type (t1->t2->t3->t4) [e1,e2]
   ==> Some ([(e1,t1);(e2,t2)], t3->t4) *)
(* match a function type t with its parameters args *)
(* and return its residual *)
(* extr_arg_type (t1->t2->t3->t4) [e1,e2] ==> Some ([(e1,t1);(e2,t2)], t3->t4) *)
(* use test harness below and run ./splt *)
let extr_arg_type (t:sPL_type) (args:'a list) : (('a * sPL_type) list * sPL_type) option =
  let rec aux env t args =
    match args,t with
      | [],_ -> Some (env,t)
      | v::vs,Arrow (t1,t2) -> aux (env@[(v,t1)]) t2 vs
      | _,_ -> None
  in aux [] t args

let rec eq_type t1 t2 =
    match t1,t2 with
        | Arrow (t1, t2), Arrow(t3, t4) -> (eq_type t1 t3) && (eq_type t2 t4)
        | t1, t2 -> t1 == t2

let extr_arg_type_test (t:sPL_type) (args:int list) : ((int * sPL_type) list * sPL_type) option =
  let pr1 = string_of_sPL_type in
  let pr2 = pr_list string_of_int in
  let pr2a = pr_list (pr_pair string_of_int pr1) in
  let pr3 = pr_option (pr_pair pr2a pr1) in
  Debug.no_2 "extr_arg_type_test" pr1 pr2 pr3 extr_arg_type t args

(* test harness below to debug extr_arg_type *)
(* please comment them after fixing bug *)
(* let () = y_binfo_pp "Testing extr_arg_type_test\n";; *)
(* let t1 = Arrow (IntType,Arrow (BoolType,IntType)) *)
(* let _ = x_add extr_arg_type_test t1 [1] *)
(* let _ = x_add extr_arg_type_test t1 [1;2] *)
(* let _ = x_add extr_arg_type_test t1 [1;2;3] *)

(* type checking method *)
(* you may use this method to check that the your inferred *)
(* type is correct *)
let type_check (env:env_type) (e:sPL_expr) (t:sPL_type) : bool =
  let rec aux env e t =
    match e with
      | IntConst _ ->
            if t=IntType then true else false
      | BoolConst _ ->
            if t=BoolType then true else false
      | Var v ->
            (match get_type env v with
              | Some t2 -> t=t2
              | None -> false (* failwith ("type-check : var "^v^" cannot be found") *)
            )
      | UnaryPrimApp (op,arg) ->
            begin
              match op,t with
                | "~",IntType
                      -> aux env arg IntType
                | "\\",BoolType
                      -> aux env arg BoolType
                | _,_
                      -> false
            end
      | BinaryPrimApp (op,arg1,arg2) ->
            begin
              match op,t with
                | "+",IntType | "-",IntType | "*",IntType | "/",IntType
                      -> (aux env arg1 IntType) && (aux env arg2 IntType)
                | "<",BoolType | ">",BoolType | "=",BoolType
                      -> (aux env arg1 IntType) && (aux env arg2 IntType)
                | "|",BoolType | "&",BoolType
                      -> (aux env arg1 BoolType) && (aux env arg2 BoolType)
                | _,_ -> false
            end
      | Cond (e1,e2,e3) ->
            let b1 = aux env e1 BoolType in
            let b2 = aux env e2 t in
            let b3 = aux env e3 t in b1 && b2 && b3
      | Func (te,args,body) ->
            if te=t then
              match extr_arg_type te args with
                | Some (env2,t2) -> aux (env2@env) body t2
                | None -> false (* mismatch in number of arguments *)
            else false
      | RecFunc (te,id,args,body) ->
            if te=t then
              match extr_arg_type te args with
                | Some (env2,t2) -> aux ((id,te)::env2@env) body t2
                | None -> false (* mismatch in number of arguments *)
            else false
      | Appln (e1,t1,args) ->
            begin
              match t1 with
                | Some t1a ->
                      begin
                        match extr_arg_type t1a args with
                          | Some (l2,t2) ->
                                if t=t2 then List.for_all (fun (ea,ta) -> aux env ea ta) l2
                                else false
                          | None -> false
                      end
                | None -> failwith "missing type : should call type_infer first"
            end
      | Let (ldecl,te,body) ->
            if te=t then
              let env2 = List.map (fun (t,v,b) -> (v,t)) ldecl in
              let nenv = env2@env in
              (aux nenv body te) && List.for_all (fun (t,_,b) -> aux nenv b t) ldecl
            else false
  in aux env e t

(* type inference, note that None is returned  *)
(*    if no suitable type is inferred *)
let rec type_infer_x (env:env_type) (e:sPL_expr) : sPL_type option * sPL_expr =
  match e with
    | IntConst _ -> (Some IntType,e)
    | BoolConst _ -> (Some BoolType,e)
    | Var v -> (get_type env v,e)
    | UnaryPrimApp (op,arg) ->
          begin
            match op with
              | "~" ->
                    let (at2,na2) = type_infer_x env arg in
                    (match at2 with
                      | Some IntType -> (at2, UnaryPrimApp (op,na2))
                      | _ -> (None,e))
              | "\\" ->
                    let (at2,na2) = type_infer_x env arg in
                    (match at2 with
                      | Some BoolType -> (at2, UnaryPrimApp (op,na2))
                      | _ -> (None,e))
              | _ -> (None,e)
          end
    | BinaryPrimApp (op,arg1,arg2) ->
          begin
            match op with
              | "-" | "+" | "*" | "/"  ->
                    let (at1,na1) = type_infer_x env arg1 in
                    let (at2,na2) = type_infer_x env arg2 in
                    (match at1,at2 with
                      | Some IntType,Some IntType -> (at2, BinaryPrimApp (op,na1,na2))
                      | _ -> (None,e))
              | "<" | ">" | "=" ->
                    let (at1,na1) = type_infer_x env arg1 in
                    let (at2,na2) = type_infer_x env arg2 in
                    (match at1,at2 with
                      | Some IntType,Some IntType -> (Some BoolType, BinaryPrimApp (op,na1,na2))
                      | _ -> (None,e))
              | "&" | "|" ->
                    let (at1,na1) = type_infer_x env arg1 in
                    let (at2,na2) = type_infer_x env arg2 in
                    (match at1,at2 with
                      | Some BoolType,Some BoolType -> (Some BoolType, BinaryPrimApp (op,na1,na2))
                      | _ -> (None,e))
              | _ -> (None,e)
          end
    | Cond (e1,e2,e3) ->
          (* e1 must be bool type *)
          (* e2,e3 must be of the same inferred type *)
            let t1,na1 = type_infer_x env e1 in
            let t2,na2 = type_infer_x env e2 in
            let t3,na3 = type_infer_x env e3 in
            (match t1, t2, t3 with
                | Some BoolType, Some t2, Some t3 ->
                        if eq_type t2 t3
                        then (Some t2, Cond(na1, na2, na3))
                        else (None, e)
                | _ -> (None, e))
    | Func (te,args,body) ->
          (* te is the inferred function type *)
          (* infer the types of args and body *)
          (* args and body type must be consistent with te *)
          (* extend the env when checking type of body *)
            (match extr_arg_type te args with
                | Some (t_arg, t_re) ->
                    let nenv = t_arg@env in
                    let nt1, nbody = type_infer_x nenv body in
                    (match nt1 with
                        | Some t1 ->
                                if eq_type t1 t_re
                                then (Some te, Func(te, args, nbody))
                                else (None, e)
                        | None -> (None, e))
                | None -> (None, e))
    | RecFunc (te,id,args,body) ->
          (* te is the inferred function type *)
          (* infer the types of args and body *)
          (* args and body type must be consistent with te*)
          (* extend the env when checking type of body *)
            (match extr_arg_type te args with
                | Some (t_arg, t_re) ->
                    let nenv = t_arg@[(id, te)]@env in
                    let nt1, nbody = type_infer_x nenv body in
                    (match nt1 with
                        | Some t1 ->
                                if eq_type t1 t_re
                                then (Some te, RecFunc(te, id, args, nbody))
                                else (None, e)
                        | None -> (None, e))
                | None -> (None, e))
    | Appln (e1,_,args) -> (* infer the type of e1 first *)
          (* infer the types of args *)
          (* check that args are consistent with inferred type *)
          (* remember to update _ with inferred type of e1 *)
          let nt1, ne = type_infer_x env e1 in
          let rec match_args seq =
              match seq with
              | ((None, _), _)::xs -> false
              | ((Some t1, _), t2)::xs -> eq_type t1 t2 && match_args xs
              | [] -> true | _ -> false
          in
          (match nt1 with
            | Some t1 ->
                    (match extr_arg_type t1 args with
                        | Some (t_arg, t_re) ->
                              let targs = List.map (fun (e, t) -> ((type_infer_x env e), t)) t_arg in
                              let nargs = List.map (fun ((_, e), _) -> e) targs in
                              if match_args targs
                              then
                                  (Some t_re, Appln(ne, Some t1, nargs))
                              else
                                  (None, e)
                        | None -> (None, e))
            | None -> (None, e))
    | Let (ldecl,te,body) ->
          (* the implementation for Let is given *)
          (* pick the type of local vars from ldecl *)
          let env2 = List.map (fun (t,v,b) -> (v,t)) ldecl in
          (* build an extended type environment for checking body *)
          let nenv = env2@env in
          (* infer the type of body *)
          let (nt1,nbody) = type_infer_x nenv body in
          (* infer the type of local definitions *)
          let ls_res = List.map (fun (t,v,b) -> (type_infer_x env b,v,t)) ldecl in
          (* why did we use env rather than nenv when checking ldecl? *)
          begin
            match nt1 with
              | Some t1 ->
                    (* check that body type is consistent *)
                    if t1=te then
                      (* check that local declarations are typed consistently *)
                      if List.for_all (fun ((t,e),_,t2) -> t=Some t2) ls_res then
                        (nt1, Let(List.map (fun ((_,e),v,t)->(t,v,e)) ls_res,te,nbody))
                      else (None,e)
                    else (None,e)
              | None -> (None,e)
          end

let rec type_infer (env:env_type) (e:sPL_expr) : sPL_type option * sPL_expr =
  Debug.no_1 "type_infer" pr_none pr_none (fun _ -> type_infer_x env e) e

(* number of arguments for full application *)
(* Ex: num_of_arg (int->(int->int)->int) ==> 2 *)
let rec num_of_arg rt =
  match rt with
    | Arrow (_,t2) -> 1+(num_of_arg t2)
    | _ -> 0

(* determine if sufficient argument for type *)
(* if insufficient - return fresh id and residual type *)
(* get_partial int->int->int [2] ===> Some (["_tmp_1"],int->int *)
(* get_partial int->int->int [] ===> Some (["_tmp_1";"_tmp_2"],int->int->int *)
let get_partial (t:sPL_type) (args:'b list) =
  if not(!pa_removal_flag) then None
  else
  match extr_arg_type t args with
    | None -> None
    | Some (ls,rt) ->
          let narg = num_of_arg rt in
          if narg=0 then None
            else Some (rt,(names # fresh_strs "_pa_var" narg))


let rec build_type ls bt =
  match ls with
    | [] -> bt
    | (t,_,_)::ls -> Arrow(t,build_type ls bt)


(*
   preprocessing to remove
    (i) partial application
    (ii) let construct
   S.sPL_expr --> C.sPL_expr
*)
let trans_exp (e:sPL_expr) : C.sPL_expr  =
  let rec aux e =
    match e with
    | BoolConst v -> C.BoolConst v
    | IntConst v -> C.IntConst v
    | Var v -> C.Var v
    | UnaryPrimApp (op,arg) ->
      let varg = aux arg in
      (C.UnaryPrimApp (op,varg))
    | BinaryPrimApp (op,arg1,arg2) ->
      let varg1 = aux arg1 in
      let varg2 = aux arg2 in
      (C.BinaryPrimApp (op,varg1,varg2))
    | Cond (e1,e2,e3) ->
      let v1 = aux e1 in
      let v2 = aux e2 in
      let v3 = aux e3 in
      C.Cond (v1,v2,v3)
    | Func (t,vs,body) ->
      let nbody = aux body in
      C.Func (t,vs,nbody)
    | RecFunc (f,t,vs,body) ->
      let nbody = aux body in
      C.RecFunc (f,t,vs,nbody)
    | Appln (f,t,args) ->
      begin
        match t with
        | Some t1 ->
          begin
            let args = List.map aux args in
            let f = aux f in
            match get_partial t1 args with
            | None ->  C.Appln (f,t1,args)
            | Some (t2,ns) -> C.Func(t2,ns,C.Appln(f,t1,args@(List.map (fun v -> C.Var v) ns)))
          end
        | _ -> failwith "missing type : not possible"
      end
    | Let (ls,t,body) ->
      (* transform Let into a function application *)
      (* build a correct type for the function from *)
      (* the type of arguments (local vars) and body *)
      let rec type_builder ls t =
          match ls, t with
            | [t1], _ -> Arrow(t1, t)
            | v::vs, _ ->
                    let nt2 = type_builder vs t in
                    Arrow(v, nt2)
      in
      let ids = List.map (fun (_, id, _) -> id) ls in
      let expr = List.map (fun (_ , _, e) -> aux e) ls in
      let types = List.map (fun (t, _, _) -> t) ls in
      let body_expr = aux body in
      let func_type = type_builder types t in
      let func = C.Func(func_type, ids, body_expr) in
      C.Appln(func, func_type, expr)

  in aux e

(* calling sPL parser *)
(*let parse_file (filename:string) : (string * sPL_expr) =
  SPL_parser.parse_file filename *)

(* set up for command argument
   using Sys and Arg modules *)
(* let usage = "usage: " ^ Sys.argv.(0) ^ " <filename>" *)
(* let file = ref ""  *)


(* Extra Assignment for 10% Bonus *)
(*
    Currently types are given at the following
    places (or features):
     (i) body of let
     (ii) local definitions of let
     (iii) function definition
     (iv) recursive function definition
    The extra assignment requires you to make their
    type declaration optional. I suggest you do them
    gradually, starting with (i), then (ii) etc.
    You must do the following for each:
     (a) change the corresponding type of each
         feature to option type in sPL.ml
     (b) change parser to make the type declaration
          optional for those features
     (c) change type_infer to infer types when not given
     (d) core language in sPLc.ml must have fully inferred type.
*)
