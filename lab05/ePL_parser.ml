
open Camlp4
open PreCast
open EPL


let expression = Gram.Entry.mk "expression"

EXTEND Gram
  GLOBAL: expression ;
  expression:
       [ LEFTA  
       	 [e1 = SELF;"|"; e2 = SELF -> BinaryPrimApp ("|",e1,e2)]
	| LEFTA
	  [e1 = SELF;  "&";e2 = SELF -> BinaryPrimApp ("&",e1,e2)]
 	|   "Equality" LEFTA 
            [e1 = SELF; "=";e2 = SELF -> BinaryPrimApp ("=",e1,e2)]
        |  "Compare" LEFTA
           [e1 = SELF; "<";e2 = SELF -> BinaryPrimApp ("<",e1,e2)
        |   e1 = SELF;  ">";e2 = SELF -> BinaryPrimApp (">",e1,e2)]
        |  "Sub Add" LEFTA
          [ e1 = SELF;"-";  e2 = SELF -> BinaryPrimApp ("-",e1,e2)
          | e1 = SELF; "+"; e2 = SELF -> BinaryPrimApp ("+",e1,e2) ]
        | "Mul Div" LEFTA
          [ e1 = SELF; "*"; e2 = SELF -> BinaryPrimApp ("*",e1,e2) 
          | e1 = SELF; "/";  e2 = SELF -> BinaryPrimApp ("/",e1,e2) ]
        | "Bracket" LEFTA
           ["("; e=SELF; ")" -> e]
        | "Unary" NONA
          [ "~"; e=SELF -> UnaryPrimApp("~",e)
          | "\\"; e=SELF -> UnaryPrimApp("\\",e)
          ]
        | "Constant" NONA 
          [ `INT (i, _) -> IntConst(i)
          | "true" -> BoolConst(true)
          | "false" -> BoolConst(false)
          ]
      ];
  END

let _loc = Loc.mk "<string>" 

(* parse from a string *)
let parse str =
  let e = Gram.parse_string expression _loc str in e 


(* parse from a file *)
let parse_file (filename:string) : (string * ePL_expr) =
  let contents =
    let lines = ref [] in
    let chan = open_in filename in
    try
      while true; do
        lines := input_line chan :: !lines
      done; []
    with End_of_file ->
        close_in chan;List.rev !lines in
  let str = String.concat "" contents in
  (str, parse str)
