include ../Makefile

SOURCES= rnd.ml

test: vanilla my
	./vanilla
	./my

my: $(SOURCES)
	$(mycaml) -o $@ unix.cmxa $(SOURCES)

vanilla: $(SOURCES)
	$(vanilla) -o $@ unix.cmxa $(SOURCES)


# generic rules :
#################

.SUFFIXES: .mll .mly .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(OCAMLPP) $(COMPFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLPP) $(COMPFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLPP) $(COMPFLAGS) -c $<

.mll.ml:
	$(OCAMLLEX) $<

.mly.ml:
	$(OCAMLYACC) -v $<

.mly.mli:
	$(OCAMLYACC) -v $<

beforedepend::

depend: beforedepend
	ocamldep $(INCLUDES) *.mli *.ml > .depend

clean::
	rm -f *.cm* *.annot *.o my vanilla
