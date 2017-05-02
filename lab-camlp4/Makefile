OCAMLC=ocamlc.opt
OCAMLOPT=ocamlopt.opt
OCAMLDEP=ocamldep
OCAMLDOC=ocamldoc
DIRS=.
INCLUDES=-annot -I +camlp4 camlp4lib.cma -pp camlp4of.opt 
GUIINCLUDES=-I +lablgtk2
OCAMLFLAGS=  $(INCLUDES) # add other options for ocamlc here
GUIOCAMLFLAGS= $(OCAMLFLAGS) $(GUIINCLUDES) #
OCAMLOPTFLAGS= -dtypes $(INCLUDES) # add other options for ocamlopt here
# removed -p from above as it seems related to profiling..
MAIN_FILES=pa_calculator.cmo


soln: soln2_code_gen.cmo soln1_calc.cmo  soln3_code_gen.cmo
# ext_float.cmo pa_extend.cmo

ex1: ex1_calc.cmo 
ex2: ex2_code_gen.cmo
ex3: ex3_code_gen.cmo
ex4: ex4_ext_float.cmo

all: ex1 ex2 ex3 ex4

ex1-t:
	@echo "Please type an expression, e.g: 1+2*3"
	camlp4 -parser ex1_calc.cmo

ex2-t:
	@echo "Please type a vector expresion, e.g: [1,2]*2"
	camlp4 -parser ex2_code_gen.cmo

ex3-t:
	@echo "Please type a vector expresion, e.g: [1,2]*2"
	./ex3e.sh

ex4-t:
	@echo "Transforming OCaml Code of test3.ml"
	@echo "===>"
	camlp4o ex4_ext_float.cmo test3.ml


ex1-st:
	@echo "Please type an expression, e.g: 1+2*3"
	camlp4 -parser soln1_calc.cmo

ex2-st:
	@echo "Please type a vector expresion, e.g: [1,2]*2"
	camlp4 -parser soln2_code_gen.cmo

ex3-st:
	@echo "Please type a vector expresion, e.g: [1,2]*2"
	./ex3e.sh



.SUFFIXES: .ml .mli .cmo .cmi .cmx .mly .mll

.ml.annot:
	$(OCAMLC) $(OCAMLFLAGS) -c -g $<

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c -g $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c -g $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

clean:
	rm *.cmo *.cmi *.annot


