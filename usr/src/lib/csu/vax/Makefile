#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.5 (Berkeley) %G%
#
#	crt0	Normal C run time startoff
#	mcrt0	C run time start off for profiling, ``prof'' conventions
#	gcrt0	C run time start off for profiling, ``gprof'' conventions
#
DESTDIR=
SRCS=	crt0.c mon.c gmon.c
OBJS=	crt0.o mcrt0.o gcrt0.o mon.o gmon.o
TAGSFILE=tags

all: ${OBJS}

install: ${OBJS}
	install -o bin -g bin -m 444 crt0.o ${DESTDIR}/lib/crt0.o
	install -o bin -g bin -m 444 mcrt0.o ${DESTDIR}/lib/mcrt0.o
	install -o bin -g bin -m 444 gcrt0.o ${DESTDIR}/usr/lib/gcrt0.o

crt0.o:	crt0.c
	${CC} -S ${DFLAGS} -DCRT0 crt0.c
	/lib/cpp crt0.s > x.s
	as -o x.o x.s
	ld -x -r -o crt0.o x.o
	rm -f x.s x.o crt0.s

moncrt0.o: crt0.c
	${CC} -S ${DFLAGS} -DMCRT0 crt0.c
	/lib/cpp crt0.s > x.s
	as -o x.o x.s
	ld -x -r -o moncrt0.o x.o
	rm -f x.s x.o crt0.s

gcrt0.o: moncrt0.o gmon.o
	ld -x -r -o gcrt0.o moncrt0.o gmon.o

mcrt0.o: moncrt0.o mon.o
	ld -x -r -o mcrt0.o moncrt0.o mon.o

mon.o: mon.c mon.ex
	${CC} -S ${DEFS} ${DFLAGS} mon.c
	ex - mon.s < mon.ex
	as -o x.o mon.s
	ld -x -r -o mon.o x.o
	rm -f x.o mon.s

gmon.o: gmon.c gmon.h gmon.ex
	${CC} -S ${DEFS} ${DFLAGS} gmon.c
	ex - gmon.s < gmon.ex
	as -o x.o gmon.s
	ld -x -r -o gmon.o x.o
	rm -f x.o gmon.s

tags:
	cwd=`pwd`; \
	for i in ${SRCS}; do \
		ctags -a -f ${TAGSFILE} $$cwd/$$i; \
	done

clean:
	rm -f ${OBJS} *.o *.s core errs tags

lint:	crt0.c
	lint crt0.c -DMCRT0
	lint crt0.c -DGCRT0
	lint crt0.c -DCRT0

depend:
	mkdep ${CFLAGS} ${SRCS}

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

crt0.o: crt0.c
mon.o: mon.c
gmon.o: gmon.c
gmon.o:gmon.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
