# Copyright (c) 1982 Regents of the University of California
#
#	@(#)Makefile	4.5	(Berkeley)	%G%
#
DESTDIR=
CFLAGS= -O

all: rrestor restor

restor: restor.c
	cc ${LDFLAGS} ${CFLAGS} -o restor restor.c

rrestor: dumprmt.o rrestor.o
	cc ${LDFLAGS} -o rrestor dumprmt.o rrestor.o -ljobs

rrestor.o: restor.c
	cc ${CFLAGS} -c -DRRESTOR restor.c
	mv restor.o rrestor.o

install: rrestor restor
	install -m 4755 rrestor ${DESTDIR}/etc/rrestor
	install restor ${DESTDIR}/etc/restor

clean:
	rm -f restor rrestor *.o errs
