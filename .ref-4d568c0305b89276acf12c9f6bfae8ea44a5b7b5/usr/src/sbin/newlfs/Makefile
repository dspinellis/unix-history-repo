#	@(#)Makefile	8.1 (Berkeley) %G%

PROG=	newlfs
CFLAGS+=-I/sys/ufs/lfs
SRCS=	dkcksum.c lfs.c lfs_cksum.c misc.c newfs.c
MAN8=	newlfs.0
.PATH:	/sys/ufs/lfs ${.CURDIR}/../disklabel

.include <bsd.prog.mk>
