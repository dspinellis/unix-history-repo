#	@(#)Makefile	5.1 (Berkeley) %G%

PROG=	mkastosc
SRCS=	mkastosc.c dohits.c asc_ebc.c ebc_disp.c
CFLAGS+=-I${.CURDIR}/../mkhits -I${.CURDIR}/.. -I.
NOMAN=	noman
.PATH:	${.CURDIR}/../mkhits ${.CURDIR}/../../api

.include <bsd.prog.mk>
