#	@(#)Makefile	5.1 (Berkeley) %G%

CFLAGS=	-O -DLIBC_SCCS
OBJS=	crt0.o gcrt0.o
CLEANFILES+=	gmon.o mon.o moncrt0.o core a.out

all: ${OBJS}

crt0.o: crt0.c
	${CC} ${CFLAGS} -c -DCRT0 ${.ALLSRC}
	${LD} -x -r ${.TARGET}
	mv a.out ${.TARGET}

moncrt0.o: crt0.c
	${CC} ${CFLAGS} -c -DMCRT0 ${.ALLSRC} -o ${.TARGET}
	${LD} -x -r ${.TARGET}
	mv a.out ${.TARGET}

gcrt0.o: moncrt0.o gmon.o mcount.o
	${LD} -x -r -o ${.TARGET} moncrt0.o gmon.o mcount.o

gmon.o:	${.CURDIR}/../csu.gmon/gmon.c
	${CC} ${CFLAGS} -c ${DEFS} ${.CURDIR}/../csu.gmon/gmon.c
	${LD} -x -r ${.TARGET}
	mv a.out ${.TARGET}

mcount.o: ${.CURDIR}/../csu.gmon/mcount.c
	${CC} ${CFLAGS} -c ${DEFS} ${.CURDIR}/../csu.gmon/mcount.c
	${LD} -x -r ${.TARGET}
	mv a.out ${.TARGET}

install:
	install -o ${BINOWN} -g ${BINGRP} -m 444 ${OBJS} \
	    ${DESTDIR}/usr/lib

depend lint tags:

.include <bsd.prog.mk>
