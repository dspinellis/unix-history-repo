#	@(#)Makefile	5.17 (Berkeley) %G%

CFLAGS=	-O -DLIBC_SCCS
OBJS=	crt0.o gcrt0.o
CLEANFILES+=	mcount.o moncrt0.o core a.out

all: ${OBJS}

crt0.o: crt0.c
	${CC} ${CFLAGS} -c -DCRT0 ${.ALLSRC}
	${LD} -x -r ${.TARGET}
	mv a.out ${.TARGET}

moncrt0.o: crt0.c
	${CC} ${CFLAGS} -c -DMCRT0 ${.ALLSRC} -o ${.TARGET}
	${LD} -x -r ${.TARGET}
	mv a.out ${.TARGET}

gcrt0.o: moncrt0.o mcount.o
	${LD} -x -r -o ${.TARGET} moncrt0.o mcount.o

mcount.o: ../csu.gmon/gmon.c ../csu.gmon/gmon.h
	${CC} ${CFLAGS} -c ${DEFS} ${.IMPSRC}
	${LD} -x -r ${.TARGET}
	mv a.out ${.TARGET}

install:
	install -o ${BINOWN} -g ${BINGRP} -m 444 ${OBJS} \
	    ${DESTDIR}/usr/lib

depend lint tags:

.include <bsd.prog.mk>
