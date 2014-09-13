CAMLI=boot/ocamlrun boot/ocamlopt -nostdlib -I boot -g

make world opt -j 2
find . -name "*.cmi" -exec rm {} \;

find . -name "*.ml" -exec name= `basename {} .ml`.mli; if [ -f name ]; then echo ""; else $CAMLI name

# to make cmi files which has not mli
boot/ocamlrun boot/ocamlopt -nostdlib -I stdlib -I bytecomp -I otherlibs/dynlink -I boot -g -c stdlib/stdlib.cmx bytecomp/opcodes.ml

make opt.opt -j 2
