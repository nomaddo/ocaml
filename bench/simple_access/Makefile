include ../Makefile

SOURCES=a.ml b.ml

test: vanilla my
	./vanilla
	./my

my: $(SOURCES)
	$(mycaml) -c a.ml
	$(mycaml) -c b.ml
	$(mycaml) -o $@ unix.cmxa a.cmx b.cmx

vanilla: $(SOURCES)
	$(vanilla) -c a.ml
	$(vanilla) -c b.ml
	$(vanilla) -o $@ unix.cmxa a.cmx b.cmx

clean:
	rm -f *.cm? *.o *.annot my vanilla
