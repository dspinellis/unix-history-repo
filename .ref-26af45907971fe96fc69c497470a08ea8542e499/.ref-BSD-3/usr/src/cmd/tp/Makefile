CFLAGS=-O
h = tp.h
o  = tp0.o tp1.o tp2.o tp3.o

tp :	$o
	$(CC) -o tp $o

$o :	$h

install :
	install -s tp $(DESTDIR)/bin

clean : 
	rm -f *.o
