#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.4 (Berkeley) %G%
#
DESTDIR=
HDRS=		robot.h
CFILES=		extern.c init_field.c main.c make_level.c move.c \
		move_robs.c play_level.c query.c rnd_pos.c score.c \
		flush_in.c
OBJS=		extern.o init_field.o main.o make_level.o move.o \
		move_robs.o play_level.o query.o rnd_pos.o score.o \
		flush_in.o
DEFS=		-DMAX_PER_UID=5
CFLAGS=		-O ${DEFS}

robots: ${OBJS}
	${CC} ${CFLAGS} -o robots ${OBJS} -lcurses -ltermlib

lint:
	lint -hb ${DEFS} ${CFILES} -lcurses 2>1 > lint.out

install: robots
	install -s -m 4711 -o daemon robots ${DESTDIR}/usr/games
	install -c -m 644 -o daemon /dev/null ${DESTDIR}/usr/games/lib/robots_roll

clean:
	rm -f a.out core *.o robots lint.out errs

depend:
