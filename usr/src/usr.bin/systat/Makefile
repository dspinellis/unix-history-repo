#	@(#)Makefile	5.14 (Berkeley) %G%

PROG=	systat
CFLAGS+=-I/sys -I${.CURDIR}/../vmstat
SRCS=	cmds.c cmdtab.c disks.c fetch.c iostat.c keyboard.c vmstat.c main.c \
	mbufs.c netcmds.c netstat.c pigs.c swap.c devname.c getbsize.c
.PATH: ${.CURDIR}/../../bin/ps ${.CURDIR}/../../bin/df
LDADD=	-lcurses -ltermcap -lm -lkvm
DPADD=	${LIBCURSES} ${LIBTERMCAP} ${LIBM}
BINGRP=	kmem
BINMODE=2555

.include <bsd.prog.mk>
