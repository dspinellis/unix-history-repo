#	@(#)Makefile	8.1 (Berkeley) 6/11/93

PROG=	restore
SRCS=	 main.c interactive.c restore.c dirs.c symtab.c tape.c utilities.c
CFLAGS+=-DLETS_GET_SMALL
NOMAN=	noman
LDFLAGS=-N
.PATH:	${.CURDIR}/../../../../sbin/restore

.include <bsd.prog.mk>
