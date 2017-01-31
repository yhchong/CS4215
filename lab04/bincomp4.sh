ocamlc -annot -c VarGen.ml 
ocamlc -annot -c debug.ml 
ocamlc  -annot -c ePL.ml 
ocamlc  -annot -I +camlp4 dynlink.cma camlp4lib.cma str.cma -pp camlp4of.opt ePL_parser.ml -a -o ePL_parser.cma
ocamlc  -pp "cppo -I ../ -D TRACE" -annot VarGen.cmo str.cma debug.cmo ePL.cmo ePL_parser.cma ePL_inter.ml -o epli

