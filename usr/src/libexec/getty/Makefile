#	@(#)Makefile	5.9 (Berkeley) %G%

PROG=	getty
SRCS=	main.c init.c subr.c gettytab.c get_date.c ttydefaults.c
LDADD=	-lutil
MAN5=	gettytab.0 ttys.0
MAN8=	getty.0

beforeinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${.CURDIR}/gettytab \
	    ${DESTDIR}/etc/gettytab

.include <bsd.prog.mk>
