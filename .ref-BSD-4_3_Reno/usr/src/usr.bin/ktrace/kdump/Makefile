#	@(#)Makefile	5.1 (Berkeley) 5/11/90

PROG=	kdump
CFLAGS+=-I${.CURDIR}/../ktrace
SRCS=	kdump.c ioctl.c subr.c
.PATH:	${.CURDIR}/../ktrace
CLEANFILES+=ioctl.c

ioctl.c: mkioctls
	/bin/sh ${.CURDIR}/mkioctls > ioctl.c

.include "../../Makefile.inc"
.include <bsd.prog.mk>
