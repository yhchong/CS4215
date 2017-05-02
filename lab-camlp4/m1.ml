(* this header file would be added to m1.ml *)
let print_seq xs =
    let rec helper xs i = match xs with
        | x::xs -> (if i > 0 then Printf.printf ", " else Printf.printf ""); print_float x;helper xs (i+1)
        | [] -> Printf.printf "]" in
    Printf.printf "[";
    helper xs 0 in
let res = 3. *. 4. in print_float res;;
