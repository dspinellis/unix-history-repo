#
# Copyright (c) 1987 Regents of the University of California.
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
DEFS=	-DMAX_PER_UID=5
CFLAGS=	-O ${DEFS}
LIBC=	/lib/libc.a
LIBS=	-lcurses -ltermlib
SRCS=	extern.c init_field.c main.c make_level.c move.c move_robs.c \
	play_level.c query.c rnd_pos.c score.c flush_in.c
OBJS=	extern.o init_field.o main.o make_level.o move.o move_robs.o \
	play_level.o query.o rnd_pos.o score.o flush_in.o
MAN=	robots.0

all: robots

robots: ${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS} ${LIBS}

clean:
	rm -f ${OBJS} core robots

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep ${CFLAGS} ${SRCS}

install: ${MAN}
	install -s -o games -g bin -m 4700 robots ${DESTDIR}/usr/games/hide
	(cd ${DESTDIR}/usr/games; rm -f robots; ln -s dm robots; chown games.bin robots)
	install -c -o games -g bin -m 600 /dev/null ${DESTDIR}/usr/games/lib/robots_roll
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat6

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}
