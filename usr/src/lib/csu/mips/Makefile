#	@(#)Makefile	5.2 (Berkeley) %G%

CFLAGS=	-O -DLIBC_SCCS
OBJS=	crt0.o gcrt0.o
CLEANFILES+=	core a.out

all: ${OBJS}

crt0.o: crt0.s
	${CPP} -DCRT0 ${.ALLSRC} | ${AS} -o $@

gcrt0.o: crt0.s
	${CPP} -DMCRT0 ${.ALLSRC} | ${AS} -o $@

install:
	install -o ${BINOWN} -g ${BINGRP} -m 444 ${OBJS} \
	    ${DESTDIR}/usr/lib

depend lint tags:

.include <bsd.prog.mk>
