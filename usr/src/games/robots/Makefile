#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.5	(Berkeley)	%G%
#
DEFS=	-DMAX_PER_UID=5
CFLAGS=	-O ${DEFS}
LIBC=	/lib/libc.a
LIBS=	-lcurses -ltermlib
SRCS=	extern.c init_field.c main.c make_level.c move.c move_robs.c \
	play_level.c query.c rnd_pos.c score.c flush_in.c
OBJS=	extern.o init_field.o main.o make_level.o move.o move_robs.o \
	play_level.o query.o rnd_pos.o score.o flush_in.o

all: robots

robots:	${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS} ${LIBS}

clean: FRC
	rm -f ${OBJS} core robots

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o games -g bin -m 4700 robots ${DESTDIR}/usr/games/hide
	(cd ${DESTDIR}/usr/games; rm -f robots; ln -s dm robots; chown games.bin robots)
	install -c -o games -g bin -m 600 /dev/null ${DESTDIR}/usr/games/lib/robots_roll

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

extern.o: extern.c robots.h /usr/include/curses.h /usr/include/stdio.h
extern.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
extern.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
extern.o: /usr/include/setjmp.h
init_field.o: init_field.c robots.h /usr/include/curses.h /usr/include/stdio.h
init_field.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
init_field.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
init_field.o: /usr/include/setjmp.h
main.o: main.c robots.h /usr/include/curses.h /usr/include/stdio.h
main.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
main.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
main.o: /usr/include/setjmp.h /usr/include/signal.h /usr/include/ctype.h
make_level.o: make_level.c robots.h /usr/include/curses.h /usr/include/stdio.h
make_level.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
make_level.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
make_level.o: /usr/include/setjmp.h
move.o: move.c robots.h /usr/include/curses.h /usr/include/stdio.h
move.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
move.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
move.o: /usr/include/setjmp.h /usr/include/ctype.h
move_robs.o: move_robs.c robots.h /usr/include/curses.h /usr/include/stdio.h
move_robs.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
move_robs.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
move_robs.o: /usr/include/setjmp.h /usr/include/signal.h
play_level.o: play_level.c robots.h /usr/include/curses.h /usr/include/stdio.h
play_level.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
play_level.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
play_level.o: /usr/include/setjmp.h
query.o: query.c robots.h /usr/include/curses.h /usr/include/stdio.h
query.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
query.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
query.o: /usr/include/setjmp.h
rnd_pos.o: rnd_pos.c robots.h /usr/include/curses.h /usr/include/stdio.h
rnd_pos.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
rnd_pos.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
rnd_pos.o: /usr/include/setjmp.h
score.o: score.c robots.h /usr/include/curses.h /usr/include/stdio.h
score.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
score.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
score.o: /usr/include/setjmp.h /usr/include/pwd.h
flush_in.o: flush_in.c /usr/include/curses.h /usr/include/stdio.h
flush_in.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
flush_in.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
