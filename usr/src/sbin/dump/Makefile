#
# Copyright (c) 1987 The Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation,
# advertising materials, and other materials related to such
# distribution and use acknowledge that the software was developed
# by the University of California, Berkeley.  The name of the
# University may not be used to endorse or promote products derived
# from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	5.8 (Berkeley) %G%
#
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

DFLAGS=
CFLAGS=	-O ${DFLAGS}
LIBC=	/lib/libc.a
PSRCS=	/usr/include/protocols/dumprestore.h dump.h dumpmain.c \
	dumptraverse.c dumptape.c dumpoptr.c dumpitime.c unctime.c
SRCS=	dump.h dumpitime.c dumpmain.c dumpoptr.c dumprmt.c dumptape.c \
	dumptraverse.c unctime.c
OBJS=	dumpitime.o dumpmain.o dumpoptr.o dumptape.o dumptraverse.o \
	unctime.o
ROBJS=	dumpitime.o dumprmain.o dumpoptr.o dumprmt.o dumprtape.o \
	dumptraverse.o unctime.o
MAN=	dump.0 rdump.0

all: dump rdump

dump: ${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS}

rdump: ${ROBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${ROBJS}

dumprmain.o: dumpmain.c
	${CC} -c -DRDUMP ${CFLAGS} dumpmain.c -o dumprmain.o

dumprtape.o: dumptape.c
	${CC} -c -DRDUMP ${CFLAGS} dumptape.c -o dumprtape.o

clean:
	rm -f ${ROBJS} ${OBJS} rdump dump core

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep ${CFLAGS} ${SRCS}

install: ${MAN}
	install -s -o root -g tty -m 6755 rdump ${DESTDIR}/etc/rdump
	install -s -o bin -g tty -m 2755 dump ${DESTDIR}/etc/dump
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat8

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}
