#	@(#)Makefile	5.10 (Berkeley) %G%

PROG=	restore
SRCS=	main.c interactive.c restore.c dirs.c symtab.c tape.c utilities.c
ROBJS=	main.o interactive.o restore.o dirs.o symtab.o rtape.o utilities.o \
	dumprmt.o
MAN8=	restore.0 rrestore.0
.PATH:	${.CURDIR}/../dump
CLEANFILES+=dumprmt.o rtape.o rrestore

all: rrestore

rrestore: ${ROBJS} ${LIBC}
	${CC} ${CFLAGS} -o ${.TARGET} ${ROBJS}

rtape.o: tape.c ${LIBC}
	${CC} ${CFLAGS} -c -DRRESTORE ${.CURDIR}/tape.c -o ${.TARGET}

afterinstall:
	install ${STRIP} -o root -g ${BINGRP} -m 4755 rrestore \
	    ${DESTDIR}${BINDIR}

.include <bsd.prog.mk>
