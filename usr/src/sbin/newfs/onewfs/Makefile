#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	7.3 (modified)	(Berkeley)	6/2/87
#
# Build a version of newfs that constructs old format file systems.
# Not installed by default.
#
CFLAGS=	-O -I.
LIBC=	/lib/libc.a
SRCS=	newfs.c mkfs.c
OBJS=	newfs.o mkfs.o

all: onewfs

onewfs: ${OBJS} ${LIBC}
	${CC} ${CFLAGS} -o $@ ${OBJS}

clean: FRC
	rm -f ${OBJS} core onewfs

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 onewfs ${DESTDIR}/etc/onewfs

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
