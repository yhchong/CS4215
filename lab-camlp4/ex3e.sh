cat m1header.txt > m1.ml 
camlp4 -parser ex3_code_gen.cmo >> m1.ml
ocamlc m1.ml

