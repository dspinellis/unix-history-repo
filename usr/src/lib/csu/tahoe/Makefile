#	@(#)Makefile	5.9 (Berkeley) %G%

CFLAGS=	-O -DLIBC_SCCS
OBJS=	crt0.o mcrt0.o gcrt0.o

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

cleandir clean:
	rm -f ${OBJS} core a.out

depend lint tags:

.include <bsd.own.mk>
