#	@(#)Makefile	5.11 (Berkeley) %G%

PROG=	restore
LINKS=	${BINDIR}/restore ${BINDIR}/rrestore
CFLAGS+=-DRRESTORE
SRCS=	main.c interactive.c restore.c dirs.c symtab.c tape.c utilities.c \
	dumprmt.c
BINOWN=	root
BINGRP=	tty
BINMODE=6555
MAN8=	restore.0
MLINKS+=restore.0 rrestore.0
.PATH:	${.CURDIR}/../dump

all: restore

.include <bsd.prog.mk>
