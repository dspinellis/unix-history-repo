#	@(#)Makefile	5.2 (Berkeley) %G%

PROG=	lpq
CFLAGS+=-I${.CURDIR}/../common_source
SRCS=	lpq.c displayq.c common.c printcap.c
BINOWN=	root
BINGRP=	daemon
BINMODE=6555
.PATH:	${.CURDIR}/../common_source

.include "../../Makefile.inc"
.include <bsd.prog.mk>
