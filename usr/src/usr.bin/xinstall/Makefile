#	@(#)Makefile	5.6 (Berkeley) %G%

PROG=	xinstall
SRCS=	stat_flags.c xinstall.c
MAN1=	install.0
.PATH:	${.CURDIR}/../../bin/ls

install: maninstall
	install ${STRIP} -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} \
	    ${PROG} ${DESTDIR}${BINDIR}/install

.include <bsd.prog.mk>
