#	@(#)Makefile	5.3 (Berkeley) 6/29/90

PROG=	lpd
CFLAGS+=-I${.CURDIR}/../common_source
LDADD=	-lutil
MAN8=	lpd.0
SRCS=	lpd.c printjob.c recvjob.c displayq.c rmjob.c startdaemon.c \
	lpdchar.c common.c printcap.c
BINOWN=	root
BINGRP=	daemon
BINMODE=6555
.PATH:	${.CURDIR}/../common_source

.include "../../Makefile.inc"
.include <bsd.prog.mk>
