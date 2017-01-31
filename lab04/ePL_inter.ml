#include "xdebug.cppo"
open EPL
open Debug.Basic

let rec power x n =
    match n with
    | 0 -> 1
    | 1 -> x
    | n -> x * power x (n-1)

(* use primitive rule to contract op[v1,v2] or op[v] to a value *)
(* raise an exception if we cannot directly contract *)
let rec contract (e:ePL_expr): ePL_expr =
  match e with
  | BoolConst _ | IntConst _ -> e
  | UnaryPrimApp (op,arg) ->
    begin
      match op with
      | "~" ->
        begin
          let () = y_tinfo_hp (add_str "inside ~ with " string_of_ePL) e in
          match arg with
          | IntConst v -> IntConst (-v)
          | _ -> failwith ("unable to contract for "^(string_of_ePL e))
        end
      | "\\" ->
        (* TODO: to negate a boolean value *)
        let () = y_tinfo_pp "inside \\ " in
        begin
          match arg with
          | BoolConst b -> BoolConst (not b)
          | _ -> failwith ("unable to contract for "^(string_of_ePL e))
        end
      | _ -> failwith ("illegal unary op "^op)
    end
  | BinaryPrimApp (op,arg1,arg2) ->
    begin
      let () = y_tinfo_hp (add_str "inside binop with " pr_id) op in
      match op with
      | "+" ->
        begin
          match arg1,arg2 with
          | IntConst v1,IntConst v2 -> IntConst (v1+v2)
          | _,_ -> failwith ("unable to contract "^(string_of_ePL e))
        end
      | "-" ->
        begin
          match arg1,arg2 with
          | IntConst v1,IntConst v2 -> IntConst (v1-v2)
          | _,_ -> failwith ("unable to contract"^(string_of_ePL e))
        end
      | "*" ->
        begin
          match arg1,arg2 with
          | IntConst v1,IntConst v2 -> IntConst (v1*v2)
          | _,_ -> failwith ("unable to contract"^(string_of_ePL e))
        end
      | "/" ->
        begin
          match arg1,arg2 with
          | IntConst v1,IntConst v2 -> IntConst (v1/v2)
          | _,_ -> failwith ("unable to contract"^(string_of_ePL e))
        end
      | "^" ->
        begin
          match arg1,arg2 with
          | IntConst v1,IntConst v2 -> IntConst (power v1 v2)
          | _,_ -> failwith ("unable to contract"^(string_of_ePL e))
        end
      | "|" ->
        begin
          (* please complete *)
          (* TODO: disjunction of two boolean values *)
          match arg1,arg2 with
          | BoolConst b1, BoolConst b2 -> BoolConst (b1 || b2)
          | _,_ -> failwith ("unable to contract"^(string_of_ePL e))
        end
      | "&" ->
        begin
          (* please complete *)
          (* TODO: conjunction of two boolean values *)
          match arg1,arg2 with
          | BoolConst b1, BoolConst b2 -> BoolConst (b1 && b2)
          | _,_ -> failwith ("unable to contract"^(string_of_ePL e))
        end
      | "<" ->
        (* please complete *)
        (* TODO: less than operator *)
        begin
          match arg1,arg2 with
          | IntConst v1, IntConst v2 -> BoolConst (v1 < v2)
          | _,_ -> failwith ("unable to contract"^(string_of_ePL e))
        end
      | ">" ->
        begin
        (* please complete *)
        (* TODO: greater than operator *)
          match arg1,arg2 with
          | IntConst v1, IntConst v2 -> BoolConst (v1 > v2)
          | _,_ -> failwith ("unable to contract"^(string_of_ePL e))
        end
      | "=" ->
        (* please complete with bool case *)
        (* TODO: make it polymorphic to handle also boolean values *)
        begin
          match arg1,arg2 with
          | IntConst v1,IntConst v2 -> BoolConst (v1=v2)
          | BoolConst v1,BoolConst v2 -> BoolConst (v1=v2)
          | _,_ -> failwith ("unable to contract"^(string_of_ePL e))
        end
      | _ -> failwith ("illegal binary op "^op)
    end


(* check if an expression is reducible or irreducible *)
let reducible (e:ePL_expr) : bool =
  match e with
    | BoolConst _ | IntConst _ -> false
    | UnaryPrimApp _ | BinaryPrimApp _ -> true

(* if expr is irreducible, returns it *)
(* otherwise, perform a one-step reduction *)
let rec oneStep (e:ePL_expr): ePL_expr =
  match e with
  | BoolConst _ | IntConst _ ->  e
  | UnaryPrimApp (op,arg) ->
    if reducible arg then UnaryPrimApp(op,oneStep arg)
    else contract e
  | BinaryPrimApp (op,arg1,arg2) ->
    if reducible arg1
    then BinaryPrimApp(op,oneStep arg1,arg2)
    else
    if reducible arg2
    then BinaryPrimApp(op,arg1,oneStep arg2)
    else contract e

(* keep reducing until we get a irreducible expr *)
(* or has an exception due to wrong operator or type error *)
let rec evaluate (e:ePL_expr): ePL_expr =
  if (reducible e) then evaluate (oneStep e)
  else e


(* sample expr in AST form *)
let e1 = IntConst 42
let e2 =
  BinaryPrimApp ("+",
    BinaryPrimApp("*",
      UnaryPrimApp("~",IntConst 15),
      IntConst 7),
    IntConst 2)
let e2a =
  BinaryPrimApp (">",IntConst 7,IntConst 10)
let e2b =
  BinaryPrimApp ("=",
    IntConst 10,
    BinaryPrimApp("+",IntConst 3,IntConst 7))

let e3 =
  BinaryPrimApp ("|",
    BinaryPrimApp("&",
      UnaryPrimApp("\\",BoolConst false),
      BoolConst true),
    BoolConst true)
let e4 =
  BinaryPrimApp ("+",IntConst 15,BoolConst true)
let e5 =
  BinaryPrimApp ("!",IntConst 15,BoolConst true)
let e5 =
  BinaryPrimApp (">",BoolConst false,BoolConst true)
let e6 =
  BinaryPrimApp ("*",
     BinaryPrimApp ("+",IntConst 1,IntConst 2),
     IntConst 3)
let e7 =
  BinaryPrimApp ("+",
     IntConst 1,
     BinaryPrimApp ("*",IntConst 2,IntConst 3))

(* type checking method *)
let rec type_check (e:ePL_expr) (t:ePL_type) : bool =
  match e,t with
  | IntConst _, IntType -> true
  | BoolConst _, BoolType -> true
  | UnaryPrimApp (op,arg), _ ->
    (* please complete below *)
    begin
      match op,t with
      | "~",IntType ->
        type_check arg IntType
      | "\\",BoolType
        ->
        (* TODO : complete type checking here *)
        type_check arg BoolType
      | _,_
        -> false
    end
  | BinaryPrimApp (op,arg1,arg2), _ ->
    begin
      match op,t with
      | "+",IntType | "-",IntType | "*",IntType | "/",IntType | "^",IntType ->
        (type_check arg1 IntType) && (type_check arg2 IntType)
      | "<",BoolType | ">",BoolType ->
        (* TODO : complete type checking here *)
        (type_check arg1 IntType) && (type_check arg2 IntType)
      | "=",BoolType ->
        (* TODO : complete polymorphic type checking here *)
        ((type_check arg1 IntType) && (type_check arg2 IntType)) ||
        ((type_check arg1 BoolType) && (type_check arg2 BoolType))
      | "|",BoolType | "&",BoolType ->
        (type_check arg1 BoolType) && (type_check arg2 BoolType)
      | _,_ -> false
    end
  | _, _ -> false

let type_check (e:ePL_expr) (t:ePL_type) : bool =
  Debug.no_2 "type_check" string_of_ePL string_of_ePL_type string_of_bool type_check e t

(* type inference, note that None is returned
   if no suitable type is inferred *)
let type_infer (e:ePL_expr) : ePL_type option =
  match e with
  | IntConst _ -> Some IntType
  | BoolConst _ -> Some BoolType
  | UnaryPrimApp (op,arg) ->
    begin
      match op with
      | "~" ->
        if (x_add type_check arg IntType) then Some IntType
        else None
      | "\\" ->
        (* TODO : complete type inference here *)
        if (x_add type_check arg BoolType) then Some BoolType
        else None
      | _ -> None
    end
  | BinaryPrimApp (op,arg1,arg2) ->
    begin
      match op with
      | "-" | "+" | "*" | "/" | "^" ->
        if (x_add type_check arg1 IntType) && (x_add type_check arg2 IntType)
        then Some IntType
        else None
      | "<" | ">" ->
        (* TODO : complete type inference here *)
        if (x_add type_check arg1 IntType) && (x_add type_check arg2 IntType)
        then Some BoolType
        else None
      | "=" ->
        (* TODO : complete type inference here *)
        let int_ok = (x_add type_check arg1 IntType) && (x_add type_check arg2 IntType) in
        let bool_ok = (x_add type_check arg1 BoolType) && (x_add type_check arg2 BoolType) in
        if int_ok || bool_ok
        then Some BoolType
        else None
      | "&" | "|" ->
        (* TODO : complete type inference here *)
        if (x_add type_check arg1 BoolType) && (x_add type_check arg2 BoolType)
        then Some BoolType
        else None
      | _ -> None
    end

let type_infer (e:ePL_expr) : ePL_type option =
  Debug.no_1 "type_infer" string_of_ePL (pr_option string_of_ePL_type) type_infer e

(* test driver for evaluation *)
let testCommand e =
  print_endline ("ePL expr:"^(string_of_ePL e));
  print_endline ("oneStep :"^(string_of_ePL (oneStep e)));
  print_endline ("evaluate:"^(string_of_ePL (evaluate e)))

(* test driver for type inference *)
let testType e =
  (* let s = (string_of_ePL e) in *)
  let v = x_add_1 type_infer e in
  match v with
    | Some t -> print_endline ("  inferred type : "^(string_of_ePL_type t));
    | None -> print_endline ("  type error ")

(* let _ = testCommand e2  *)
(* let _ = testCommand e2a  *)
(* let _ = testCommand e2b  *)
(* let _ = testCommand e3 *)

(* let _ = testType e1 *)
(* let _ = testType e2 *)
(* let _ = testType e2a  *)
(* let _ = testType e2b  *)
(* let _ = testType e3 *)
(* let _ = testType e4 *)
(* let _ = testType e5 *)

(* let _ = testType e6 *)
(* let _ = testCommand e6 *)
(* let _ = testType e7 *)
(* let _ = testCommand e7 *)
(* let _ = testType e4 *)
(* let _ = testCommand e4 *)

(* calling ePL parser *)
let parse_file (filename:string) : (string * ePL_expr) =
  EPL_parser.parse_file filename

(* set up for command argument
   using Sys and Arg modules *)
let usage = "usage: " ^ Sys.argv.(0) ^ " <filename>"
let file = ref ""


let testType2 e =
  (* let s = (string_of_ePL e) in *)
  let v = x_add_1 type_infer e in
  (match v with
    | Some t -> print_endline ("  inferred type : "^(string_of_ePL_type t));
    | None -> print_endline ("  type error!! "));v

(* main program *)
let main =
  (* Read the arguments of command *)
  Arg.parse Debug.command_args (fun s -> file := s) usage;
  if String.length !file == 0 then print_endline usage
  else
    let _ = print_endline "Loading ePL program .." in
    let (s,p) = parse_file !file in
    let _ = print_endline ("  "^s) in
    let _ = print_endline ("  as "^(string_of_ePL p)) in
    let _ = print_endline "Type checking program .." in
    let v = testType2 p in
    if v=None then ()
    else
      let _ = print_string "Evaluating ==> " in
      let r = evaluate p in
      print_endline (string_of_ePL r)
