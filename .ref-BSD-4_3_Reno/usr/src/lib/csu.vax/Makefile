#	@(#)Makefile	5.13 (Berkeley) 6/8/90

CFLAGS=	-O -DLIBC_SCCS
OBJS=	crt0.o mcrt0.o gcrt0.o
CLEANFILES+=	gmon.o mon.o moncrt0.o core a.out

all: ${OBJS}

crt0.o: crt0.c
	${CC} -S -DCRT0 ${.ALLSRC}
	${CPP} crt0.s > x.s
	${AS} -o x.o x.s
	${LD} -x -r -o ${.TARGET} x.o
	rm -f x.s x.o crt0.s

moncrt0.o: crt0.c
	${CC} -S -DMCRT0 ${.ALLSRC}
	${CPP} crt0.s > x.s
	${AS} -o x.o x.s
	${LD} -x -r -o ${.TARGET} x.o
	rm -f x.s x.o crt0.s

mcrt0.o: moncrt0.o mon.o
	${LD} -x -r -o ${.TARGET} moncrt0.o mon.o

gcrt0.o: moncrt0.o gmon.o
	${LD} -x -r -o ${.TARGET} moncrt0.o gmon.o

mon.o: mon.c mon.ex
	${CC} -S ${DEFS} ${.IMPSRC}
	ex - mon.s < ${.CURDIR}/mon.ex
	${AS} -o x.o mon.s
	${LD} -x -r -o ${.TARGET} x.o
	rm -f x.o mon.s

gmon.o: gmon.c gmon.h gmon.ex
	${CC} -S ${DEFS} ${.IMPSRC}
	ex - gmon.s < ${.CURDIR}/gmon.ex
	${AS} -o x.o gmon.s
	${LD} -x -r -o ${.TARGET} x.o
	rm -f x.o gmon.s

install:
	install -o ${BINOWN} -g ${BINGRP} -m 444 ${OBJS} \
	    ${DESTDIR}/usr/lib

depend lint tags:

.include <bsd.prog.mk>
