#	@(#)Makefile	5.14 (Berkeley) %G%

#	dump.h			header file
#	dumpitime.c		reads /etc/dumpdates
#	dumpmain.c		driver
#	dumpoptr.c		operator interface
#	dumprmt.c		handles remote tape via rmt(8)
#	dumptape.c		handles the mag tape and opening/closing
#	dumptraverse.c		traverses the file system
#	unctime.c		undo ctime
#
#	DEBUG			use local directory to find ddate and dumpdates
#	TDEBUG			trace out the process forking

PROG=	dump
LINKS=	${BINDIR}/dump ${BINDIR}/rdump
CFLAGS+=-DRDUMP
SRCS=	dumpitime.c dumpmain.c dumpoptr.c dumprmt.c dumptape.c \
	dumptraverse.c unctime.c
BINOWN=	root
BINGRP=	tty
BINMODE=6555
MAN8=	dump.0
MLINKS+=dump.8 rdump.8

all: dump

.include <bsd.prog.mk>
