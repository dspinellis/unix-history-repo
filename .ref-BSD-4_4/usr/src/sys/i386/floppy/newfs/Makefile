#	@(#)Makefile	8.1 (Berkeley) 6/11/93

PROG=	newfs
SRCS=	dkcksum.c newfs.c mkfs.c
CFLAGS+=-DLETS_GET_SMALL -DMFS
NOMAN=	noman
LDFLAGS=-N
.PATH:	${.CURDIR}/../../../../sbin/newfs ${.CURDIR}/../../../../sbin/disklabel

.include <bsd.prog.mk>
