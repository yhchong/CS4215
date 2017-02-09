ocamlc -annot -c VarGen.ml 
ocamlc -annot -c debug.ml 
ocamlc -annot ePL.ml 
ocamlc -annot -I +camlp4 dynlink.cma camlp4lib.cma -pp camlp4of.opt ePL_parser.ml -a -o ePL_parser.cma
ocamlc -pp "cppo -I ../ -D TRACE" -annot VarGen.cmo str.cma debug.cmo ePL.cmo ePL_parser.cma ePL_compile.ml -o eplc
ocamlc -pp "cppo -I ../ -D TRACE" -annot VarGen.cmo str.cma debug.cmo -annot ePL.cmo ePL_parser.cma eVM_exec.ml -o evm

