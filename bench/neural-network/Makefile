include ../Makefile

SOURCES=dataset.ml neuralNetwork.ml

test: my vanilla
	$(time) ./my
	$(time) ./vanilla

my: $(SOURCES)
	$(mycaml) -g -o my $(SOURCES)

vanilla: $(SOURCES)
	$(vanilla) -g -o vanilla $(SOURCES)

clean:
	rm -f my vanilla *.cm? *.o *.annot
