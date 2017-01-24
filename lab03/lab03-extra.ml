(* TREES *)

type 'a tree = Leaf of 'a | Node of 'a * ('a tree) * ('a tree)

let bt1 = Node (1,(Leaf 2),Node (3,(Leaf 4),(Leaf 5)));;

(* EXTRA PROBLEMS *)

(*
  Write a method to generate all possible binary trees
  with n number of leaves. Number the leaves, so that
  there are ordered in increasing sequence based on in-order
  traversal. The values at the nodes are left with the 0 value.

  For example:
  genTree 3 ==> [Node (0, Leaf 1, Node (0, Leaf 2, Leaf 3));
                 Node (0, Node (0, Leaf 1, Leaf 2), Leaf 3)]

  genTree 4 ==>
   [Node (0, Leaf 1, Node (0, Leaf 2, Node (0, Leaf 3, Leaf 4)));
    Node (0, Leaf 1, Node (0, Node (0, Leaf 2, Leaf 3), Leaf 4));
    Node (0, Node (0, Leaf 1, Leaf 2), Node (0, Leaf 3, Leaf 4));
    Node (0, Node (0, Leaf 1, Node (0, Leaf 2, Leaf 3)), Leaf 4);
    Node (0, Node (0, Node (0, Leaf 1, Leaf 2), Leaf 3), Leaf 4)]

  Hint : you may make use of genPairs and prod methods you
    implemented for lab3.
*)

let genTree n = failwith "Not yet implemented";;


genTree 3;;
(* val genTree : int -> int tree list = <fun> *)
(* - : int tree list = *)
(* [Node (0, Leaf 1, Node (0, Leaf 2, Leaf 3)); *)
(*  Node (0, Node (0, Leaf 1, Leaf 2), Leaf 3)] *)

(* 
  Write a method that would convert a number to its equivalent
  in English.

  You may assume that the numbers are smaller than
      999,999,999,999

   to_English 0 = "zero"
   to_English 101 = "one hundred and one"
   to_English 20111 = "twenty thousand and one hundred and eleven"
   to_English 1000000 = "one million"
*)

let to_English n =
  let bigunit = ["thousand";"million";"billion"] in
  let u = ["";"one";"two";"three";"four";"five";"six";"seven";"eight";"nine"] in
  let t2 = ["ten";"eleven";"twelve";"thirteen";"fourteen";"fifteen";"sixteen";"seventeen";"eighteen";"nineteen"] in
  let t3 = ["twenty";"thirty";"forty";"fifty";"sixty";"seventy";"eighty";"ninety"] in
  let join s1 s2 =
    if s1="" then s2
    else if s2="" then s1
    else s1^" "^s2 
  in
  if n=0 then "zero"
  else failwith "Not yet implemented"
;;

to_English 11;;
to_English 10000;;
to_English 1000000;;
(* val to_English : int -> string = <fun> *)
(* - : string = "eleven" *)
(* - : string = "ten thousand" *)
(* - : string = "one million" *)
to_English 999999999;;
to_English 0;;
