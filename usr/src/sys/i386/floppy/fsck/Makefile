#	@(#)Makefile	5.1 (Berkeley) %G%

PROG=	fsck
SRCS=	dir.c inode.c main.c pass1.c pass1b.c pass2.c pass3.c pass4.c \
	pass5.c preen.c setup.c utilities.c ufs_subr.c ufs_tables.c
CFLAGS+=-DLETS_GET_SMALL
NOMAN=	noman
LDFLAGS=-N
.PATH:	${.CURDIR}/../../../../sbin/fsck ${.CURDIR}/../../../ufs

.include <bsd.prog.mk>
