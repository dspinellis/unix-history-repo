#	@(#)Makefile	5.1 (Berkeley) 5/3/91

PROG=	disklabel
SRCS=	disklabel.c dkcksum.c
CFLAGS+=-DLETS_GET_SMALL -I${.CURDIR}/../../../../sbin/disklabel
NOMAN=	noman
LDFLAGS=-s -N
.PATH:	${.CURDIR}/../../../../sbin/disklabel

.include <bsd.prog.mk>
