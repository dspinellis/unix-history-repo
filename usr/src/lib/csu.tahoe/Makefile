#	@(#)Makefile	5.11 (Berkeley) 7/2/90

CFLAGS=	-O -DLIBC_SCCS
OBJS=	crt0.o mcrt0.o gcrt0.o
CLEANFILES=gmon.o mon.o moncrt0.o

.s.o:
	${AS} -o ${.TARGET} ${.IMPSRC}
	@${LD} -x -r ${.TARGET}
	@mv a.out ${.TARGET}

all: ${OBJS}

mcrt0.o: moncrt0.o mon.o
	${LD} -x -r -o ${.TARGET} moncrt0.o mon.o

gcrt0.o: moncrt0.o gmon.o
	${LD} -x -r -o ${.TARGET} ${.ALLSRC}

moncrt0.o: mcrt0.s
	${AS} -o ${.TARGET} ${.ALLSRC}

mon.o: mon.c mon.ex
	${CC} -S ${CFLAGS} ${.IMPSRC}
	ex - mon.s < ${.CURDIR}/mon.ex
	${AS} -o ${.TARGET} mon.s
	rm -f mon.s

gmon.o: gmon.c mon.ex
	${CC} -S ${CFLAGS} ${.IMPSRC}
	ex - gmon.s < ${.CURDIR}/mon.ex
	${AS} -o $@ gmon.s
	rm -f gmon.s

install:
	install -o ${BINOWN} -g ${BINGRP} -m 444 ${OBJS} \
	    ${DESTDIR}/usr/lib

depend lint tags:

.include <bsd.prog.mk>
