#	@(#)Makefile	5.13 (Berkeley) %G%

PROG=	ftpd
CFLAGS+=-DSETPROCTITLE
SRCS=	ftpd.c ftpcmd.c glob.c logwtmp.c popen.c vers.c
CLEANFILES+=ftpcmd.c vers.c version y.tab.h

vers.o: ftpd.c ftpcmd.y
	sh ${.CURDIR}/newvers.sh
	${CC} ${CFLAGS} -c vers.c

.include <bsd.prog.mk>
