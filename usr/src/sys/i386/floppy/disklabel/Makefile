#	@(#)Makefile	5.1 (Berkeley) %G%

PROG=	disklabel
SRCS=	disklabel.c dkcksum.c
CFLAGS+=-DLETS_GET_SMALL -I${.CURDIR}/../../../../sbin/disklabel
NOMAN=	noman
LDFLAGS=-N
.PATH:	${.CURDIR}/../../../../sbin/disklabel

.include <bsd.prog.mk>
