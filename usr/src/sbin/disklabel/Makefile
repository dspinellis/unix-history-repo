#	@(#)Makefile	5.5 (Berkeley) %G%

PROG=	disklabel
MAN8=	disklabel.0
BINGRP=operator
BINMODE=2550
CLEANFILES=disklabel.5.0

all: disklabel.5.0

beforeinstall:
	install -c -o ${MANOWN} -g ${MANGRP} -m ${MANMODE} disklabel.5.0 \
	    ${DESTDIR}${MANDIR}5/disklabel.0

.include <bsd.prog.mk>
