#	@(#)Makefile	8.1 (Berkeley) %G%

PROG=	disklabel
SRCS=	disklabel.c dkcksum.c
MAN8=	disklabel.0
BINGRP=operator
BINMODE=2550
CLEANFILES=disklabel.5.0

all: ${PROG} disklabel.5.0 ${MAN8}

beforeinstall:
	install -c -o ${MANOWN} -g ${MANGRP} -m ${MANMODE} disklabel.5.0 \
	    ${DESTDIR}${MANDIR}5/disklabel.0

.include <bsd.prog.mk>
