# Citation: microc and Matrx
# authored by: Annie, Lior and Shida

# "make test" Compiles everything and runs the regression tests

.PHONY : test
test : all testall.sh
	./testall.sh

# "make all" builds the executable

.PHONY : all
all : xirtam.native matrix.o

# "make xirtam.native" compiles the compiler
#
# The _tags file controls the operation of ocamlbuild, e.g., by including
# packages, enabling warnings
#
# See https://github.com/ocaml/ocamlbuild/blob/master/manual/manual.adoc

xirtam.native : matrix.bc
	opam config exec -- \
	ocamlbuild -use-ocamlfind xirtam.native -pkgs llvm,llvm.analysis,llvm.bitreader

# "make clean" removes all generated files

.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf testall.log ocamlllvm *.diff *.ll *.exe *.s *.o *.bc matrix

matrix : matrix.c
	cc -o matrix -DBUILD_TEST matrix.c

matrix.bc : matrix.c
	clang -emit-llvm -o matrix.bc -c matrix.c -Wno-varargs
