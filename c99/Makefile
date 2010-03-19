CFLAGS = -std=c99 -O2

VLASTE = ../share/valsi.tsv

all : stejudri.o vlaste.o vlaciha

vlaciha : vlaciha.o stejudri.o vlaste.o

stejudri.o vlaste.o vlaciha.o : stejudri.h vlaste.h

stejudri.c stejudri.h : mkindex.pl $(VLASTE)
	./mkindex.pl $(VLASTE)

clean :
	rm -f *.o stejudri.[ch] vlaciha
