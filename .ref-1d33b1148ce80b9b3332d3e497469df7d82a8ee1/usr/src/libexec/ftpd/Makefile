#	@(#)Makefile	8.2 (Berkeley) %G%

PROG=	ftpd
CFLAGS+=-DSETPROCTITLE
SRCS=	ftpd.c ftpcmd.c logwtmp.c popen.c
MAN8=	ftpd.0
CLEANFILES+=ftpcmd.c y.tab.h

.include <bsd.prog.mk>
