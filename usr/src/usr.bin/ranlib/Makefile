#	@(#)Makefile	5.4 (Berkeley) 3/12/91

PROG=	ranlib
SRCS=	archive.c build.c misc.c ranlib.c touch.c
CFLAGS+=-I${.CURDIR} -I${.CURDIR}/../ar
MAN1=	ranlib.0
VPATH=	${.CURDIR}/../ar
CLEANFILES=ranlib.5.0

ranlib.0: ranlib.5.0

afterinstall:
	install -c -o ${MANOWN} -g ${MANGRP} -m ${MANMODE} ranlib.5.0 \
	    ${DESTDIR}${MANDIR}5/ranlib.0

.include <bsd.prog.mk>
