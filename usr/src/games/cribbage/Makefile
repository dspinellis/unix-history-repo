#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.1 (Berkeley) %G%
#
# make file for cribbage
#
HDRS=	cribbage.h deck.h cribcur.h
OBJS=	extern.o crib.o support.o cards.o score.o io.o
CFILES=	extern.c crib.c support.c cards.c score.c io.c
TOBJS=	test.o cards.o score.o io.o extern.o
CFLAGS=	-O

cribbage:	${OBJS}
	${CC} ${CFLAGS} -o cribbage ${OBJS} -lcurses -ltermlib

all: cribbage crib.instr

crib.o io.o support.o: cribcur.h

test:		${TOBJS}
	${CC} ${CFLAGS} -o test ${TOBJS} -lcurses -ltermlib

crib.instr: cribbage.n macro
	nroff cribbage.n > crib.instr

tags: ${HDRS} ${CFILES}
	ctags -u $?
	ed - tags < :ctfix
	sort tags -o tags

clean:
	rm -f ${OBJS} ? a.out core crib.instr cribbage

crib.o:		deck.h	cribbage.h	cribcur.h
support.o:	deck.h	cribbage.h	cribcur.h
test.o:		deck.h
cards.o:	deck.h
score.o:	deck.h
io.o:		deck.h			cribcur.h

install: cribbage crib.instr
	install -c -s cribbage /usr/games/
	install -c crib.instr /usr/games/lib/
