#	@(#)Makefile	5.1 (Berkeley) %G%

PROG=	lpr
CFLAGS+=-I${.CURDIR}/../common_source
SRCS=	lpr.c startdaemon.c printcap.c
.PATH:	${.CURDIR}/../common_source

.include "../../Makefile.inc"
.include <bsd.prog.mk>
