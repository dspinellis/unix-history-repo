#	@(#)Makefile	5.2 (Berkeley) %G%

PROG=	lfs_cleanerd
CFLAGS+=-I/sys/ufs/lfs -I${.CURDIR}
SRCS=	cleanerd.c lfs_cksum.c library.c misc.c print.c

.PATH:	/sys/ufs/lfs

LDADD=	-lutil -lkvm
DPADD=	${LIBUTIL} ${LIBKVM}
NOMAN=	noman

.include <bsd.prog.mk>
