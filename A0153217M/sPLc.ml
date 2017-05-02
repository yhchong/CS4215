(* PLEASE DO NOT CHANGE THIS FILE *)

module S = SPL

type op_id = S.op_id
type id = S.id
type sPL_type = S.sPL_type

(* open Gen *)
open Debug


(* AST for core language *)
type sPL_expr =
  | BoolConst of bool
  | IntConst of int
  | Var of id
  | UnaryPrimApp of op_id * sPL_expr
  | BinaryPrimApp of op_id * sPL_expr * sPL_expr
  | Cond of sPL_expr * sPL_expr * sPL_expr
  | Func of sPL_type * (id list) * sPL_expr 
        (* min of one parameter *)
  | RecFunc of sPL_type * id * (id list) * sPL_expr 
        (* min of one parameter *)
  | Rel of (sPL_type * ((sPL_expr list) list))
  | Proj of (sPL_expr * (id list))
  | Appln of sPL_expr * sPL_type * (sPL_expr list) 
        (* all applications fully applied *)

(* display sPL expr in prefix form *)
(* PLEASE do not change *)
let string_of_sPL (e:sPL_expr):string =
  let pr_type t = "{"^(S.string_of_sPL_type t)^"}" in
  let rec pr_rel_type xs = match xs with
      | (i, t)::xs -> i^":"^(S.string_of_sPL_type t)^" "^(pr_rel_type xs)
      | [] -> ""
  in
  let print_spl_type t = match t with
    | S.RelType v -> pr_rel_type v
    | _ -> "" in
  let rec print_row row f = match row with
    | x::xs -> (f x)^(print_row xs f)
    | [] -> "" in
  let rec print_rows rows f = match rows with
    | row::rows -> (print_row row f)^"\n"^(print_rows rows f)
    | [] -> "" in
  let rec aux e =
  match e with
    | BoolConst v -> "Bool("^(string_of_bool v)^")"
    | IntConst v -> "Int("^(string_of_int v)^")"
    | Var v -> "Var("^v^")"
    | UnaryPrimApp (op,arg) -> op^"["^(aux arg)^"]"
    | BinaryPrimApp (op,arg1,arg2) -> op^"["^(aux arg1)^","^(aux arg2)^"]"
    | Cond (e1,e2,e3) -> "if "^(aux e1)^" then "^(aux e2)^" else "^(aux e3)
    | Func (t,args,body) -> "fun "^(pr_type t)^" "^(pr_lst " " pr_id args)^" -> "^(aux body)^" end"
    | RecFunc (t,r,args,body) -> "recfun "^r^" "^(pr_type t)^" "^(pr_lst " " pr_id args)^" -> "^(aux body)^" end"
    | Appln (e,t,args) -> "Appln["^(aux e)^"; "^(pr_lst ";" aux args)^"]"
    | Rel (e, v) -> "Rel("^(print_spl_type e)^")\n"^(print_rows v aux)
    | Proj (e, v) -> "Proj("^(aux e)^" # "^(pr_lst " " pr_id v)
  in aux e


(* free vars of an expression *)
let rec fv (e:sPL_expr) : id list =
  match e with
    | BoolConst _  | IntConst _ -> []
    | Var i -> [i]
    | UnaryPrimApp (_,arg) -> fv arg
    | BinaryPrimApp (_,arg1,arg2) -> (fv arg1)@(fv arg2)
    | Cond (e1,e2,e3) -> (fv e1)@(fv e2)@(fv e3)
    | Func (_,vs,body) -> diff (fv body) vs
    | RecFunc (_,i,vs,body) -> diff (fv body) (i::vs)
    | Appln (e1,_,es) -> (fv e1)@(List.concat (List.map fv es))


