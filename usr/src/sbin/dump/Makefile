#	@(#)Makefile	5.11 (Berkeley) 7/1/90

#	dump.h			header file
#	dumpitime.c		reads /etc/dumpdates
#	dumpmain.c		driver
#	dumpoptr.c		operator interface
#	dumptape.c		handles the mag tape and opening/closing
#	dumptraverse.c		traverses the file system
#	unctime.c		undo ctime
#
#	DEBUG			use local directory to find ddate and dumpdates
#	TDEBUG			trace out the process forking

PROG=	dump
SRCS=	dumpitime.c dumpmain.c dumpoptr.c dumptape.c dumptraverse.c \
	unctime.c
ROBJS=	dumpitime.o dumprmain.o dumpoptr.o dumprmt.o dumprtape.o \
	dumptraverse.o unctime.o
BINGRP=	tty
MAN8=	dump.0 rdump.0
CLEANFILES=dumprmain.o dumprtape.o dumprmt.o rdump

all: rdump

rdump: ${ROBJS} ${LIBC}
	${CC} -o ${.TARGET} ${CFLAGS} ${ROBJS}

dumprmain.o:
	${CC} -c -DRDUMP ${CFLAGS} ${.CURDIR}/dumpmain.c -o ${.TARGET}

dumprtape.o:
	${CC} -c -DRDUMP ${CFLAGS} ${.CURDIR}/dumptape.c -o ${.TARGET}

afterinstall:
	install ${STRIP} -o root -g ${BINGRP} -m 6555 rdump \
	    ${DESTDIR}${BINDIR}

.include <bsd.prog.mk>
