#	@(#)Makefile	8.1 (Berkeley) 6/11/93

PROG=	disklabel
SRCS=	disklabel.c dkcksum.c
CFLAGS+=-DLETS_GET_SMALL -I${.CURDIR}/../../../../sbin/disklabel
NOMAN=	noman
LDFLAGS=-N
.PATH:	${.CURDIR}/../../../../sbin/disklabel

.include <bsd.prog.mk>
