#	@(#)Makefile	5.10 (Berkeley) %G%

PROG=	systat
CFLAGS+=-I/sys
SRCS=	cmds.c cmdtab.c disks.c fetch.c iostat.c keyboard.c vmstat.c main.c \
	mbufs.c netcmds.c netstat.c pigs.c swap.c devname.c
.PATH:	${.CURDIR}/../../bin/ps
LDADD=	-lcurses -ltermcap -lm -lutil -lcompat
DPADD=	${LIBCURSES} ${LIBTERMCAP} ${LIBM} ${LIBUTIL}
BINGRP=	kmem
BINMODE=2555

.include <bsd.prog.mk>
