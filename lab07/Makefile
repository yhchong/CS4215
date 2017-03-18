OCAMLBUILD = ocamlbuild

#  number of parallel jobs, 0 means unlimited.
JOBS = 0

# dynlink should precede camlp4lib
LIBSB = dynlink,camlp4lib,unix,str
LIBSN = dynlink,camlp4lib,unix,str
LIBS2 = dynlink,camlp4lib
#CPPO_FLAGS = -pp "cppo -I ../ -D TRACE"
CPPO_FLAGS = 

INCLUDES = -I,+camlp4
 
FLAGS = $(INCLUDES),-dtypes
GFLAGS = $(INCLUDES),-g,-dtypes

# -no-hygiene flag to disable "hygiene" rules
OBB_FLAGS = -no-links -libs $(LIBSB) -cflags $(FLAGS) -lflags $(FLAGS) -lexflag -q -yaccflag -v  -j $(JOBS) $(CPPO_FLAGS)
OBBC_FLAGS = -no-links -libs $(LIBSB) -cflags $(FLAGS) -lflags $(FLAGS) -lexflag -q -yaccflag -v  -j $(JOBS) $(CPPO_FLAGS)
OBN_FLAGS = -no-links -libs $(LIBSN) -cflags $(FLAGS) -lflags $(FLAGS) -lexflag -q -yaccflag -v  -j $(JOBS) $(CPPO_FLAGS)

all: lab07

byte: lab07

# native: splt.native spli.native

lab06 : splt.byte

# lab07 : spli.byte

#lab08 : splc.byte svm.byte

#lab08b : splc.byte svm2.byte

lab07 : splc.byte svm.byte

lab07b : splc.byte svm2.byte


parser : sPL_parser.cmo

sPL_parser.cmo:
	@ocamlbuild $(OBB_FLAGS) sPL_parser.cmo

splt.byte: 
	@ocamlbuild $(OBB_FLAGS) sPL_type_m.byte
	cp -u _build/sPL_type_m.byte splt

# splt.native:
# 	@ocamlbuild $(OBN_FLAGS) sPL_type_m.native
# 	cp -u _build/sPL_type_m.native splt

spli.byte: 
	@ocamlbuild $(OBB_FLAGS) sPL_inter1.byte sPL_inter2.byte
	cp -u _build/sPL_inter1.byte spli
	cp -u _build/sPL_inter2.byte spli2

svm.byte: xdebug.cppo
	@ocamlbuild $(OBB_FLAGS) sVM_exec.byte
	cp -u _build/sVM_exec.byte svm

svm2.byte:
	@ocamlbuild $(OBB_FLAGS) sVM_exec2.byte
	cp -u _build/sVM_exec2.byte svm2

splc.byte: xdebug.cppo
	@ocamlbuild $(OBB_FLAGS) sPL_compile.byte
	cp -u _build/sPL_compile.byte splc

# spli.native:
# 	@ocamlbuild $(OBN_FLAGS) sPL_inter1.native sPL_inter2.native
# 	cp -u _build/sPL_inter1.native spli
# 	cp -u _build/sPL_inter2.native spli2

# Clean up
clean:
	$(OCAMLBUILD) -quiet -clean 
	rm -f splt spli spli2
	rm -f *cma *.cmo *.cmi *.cmx *.o *.mli *.output *.annot
	rm -f splc svm
	# rm sPL_lexer.ml
