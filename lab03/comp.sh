# interactive interpreter
# ocaml
# #use "lab03.ml";;
# #quit;;
#
# batch interpreter
ocaml lab03.ml
# bytecode compiler
ocamlc lab03.ml -o ex3.byte
ocamlrun ex3.byte
# or just ./ex3.byte
# native compiler
ocamlopt lab03.ml -o ex3
./ex3
