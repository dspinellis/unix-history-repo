#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.3	(Berkeley)	%G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	zork.c
OBJS=	zork.o

all: zork

zork: zork.c ${LIBC}
	${CC} ${CFLAGS} -o $@ zork.c

clean: FRC
	rm -f ${OBJS} core zork

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 zork ${DESTDIR}/usr/games/zork
	install -c -o bin -g bin -m 755 zorklib/dtext.dat ${DESTDIR}/usr/games/lib
	install -c -o bin -g bin -m 755 zorklib/dindex.dat ${DESTDIR}/usr/games/lib
	install -c -o bin -g bin -m 755 zorklib/doverlay ${DESTDIR}/usr/games/lib
	install -c -o bin -g bin -m 755 zorklib/dungeon ${DESTDIR}/usr/games/lib

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

zork.o: zork.c /usr/include/stdio.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
