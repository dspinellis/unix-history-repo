#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.2	(Berkeley)	%G%
#
DESTDIR=
CFLAGS=	-O
OBJS=	crt0.o mcrt0.o gcrt0.o

.s.o:
	${AS} -o $*.o $*.s
	ld -x -r $*.o
	mv a.out $*.o

all:	${OBJS}

mcrt0.o:	moncrt0.o mon.o
	ld -x -r -o mcrt0.o moncrt0.o mon.o

gcrt0.o:	moncrt0.o gmon.o
	ld -x -r -o gcrt0.o moncrt0.o gmon.o

moncrt0.o:	mcrt0.s
	$(AS) -o moncrt0.o mcrt0.s

mon.o:	mon.c
	${CC} -S ${CFLAGS} mon.c
	ex - mon.s < mon.ex
	$(AS) -o mon.o mon.s

gmon.o:	gmon.c
	${CC} -S ${CFLAGS} gmon.c
	ex - gmon.s < mon.ex
	${AS} -o gmon.o gmon.s

install: all
	install -o bin -g bin -m 444 crt0.o ${DESTDIR}/lib/crt0.o
	install -o bin -g bin -m 444 mcrt0.o ${DESTDIR}/lib/mcrt0.o
	install -o bin -g bin -m 444 gcrt0.o ${DESTDIR}/usr/lib/gcrt0.o

clean:
	rm -f ${OBJS} *mon.s moncrt0.o mon.o gmon.o a.out errs core

depend:
