#	@(#)Makefile	4.2	%G%

CFLAGS=-O
h = tp.h tp_defs.h
o  = tp0.o tp1.o tp2.o tp3.o

tp :	$o
	$(CC) -o tp $o

$o :	$h

install : tp
	install -s tp ${DESTDIR}/bin

clean : 
	rm -f *.o

depend:
