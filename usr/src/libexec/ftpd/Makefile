#	@(#)Makefile	5.12 (Berkeley) %G%

PROG=	ftpd
CFLAGS+=-I${.CURDIR}/../../../usr.bin/ftp -DSETPROCTITLE
SRCS=	ftpd.c ftpcmd.c glob.c logwtmp.c popen.c vers.c
CLEANFILES+=ftpcmd.c

vers.o: ftpd.c ftpcmd.y
	sh ${.CURDIR}/newvers.sh
	${CC} ${CFLAGS} -c vers.c

.include <bsd.prog.mk>
