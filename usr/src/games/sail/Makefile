#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.4	(Berkeley)	%G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
LIBS=	-lcurses -ltermlib
SRCS=	main.c pl_main.c pl_1.c pl_2.c pl_3.c pl_4.c pl_5.c pl_6.c pl_7.c \
	dr_main.c dr_1.c dr_2.c dr_3.c dr_4.c dr_5.c lo_main.c \
	assorted.c game.c globals.c misc.c parties.c sync.c version.c
OBJS=	main.o pl_main.o pl_1.o pl_2.o pl_3.o pl_4.o pl_5.o pl_6.o pl_7.o \
	dr_main.o dr_1.o dr_2.o dr_3.o dr_4.o dr_5.o lo_main.o \
	assorted.o game.o globals.o misc.o parties.o sync.o version.o

all: sail

sail: ${OBJS} ${LIBC}
	${CC} -o $@ ${OBJS} ${LIBS}

clean: FRC
	rm -f ${OBJS} core sail

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o daemon -g bin -m 4755 sail ${DESTDIR}/usr/games/sail
	install -c -o daemon -g bin -m 644 /dev/null ${DESTDIR}/usr/games/saillog

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

main.o: main.c externs.h /usr/include/stdio.h /usr/include/signal.h
main.o: /usr/include/ctype.h /usr/include/setjmp.h machdep.h
pl_main.o: pl_main.c player.h /usr/include/curses.h /usr/include/stdio.h
pl_main.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
pl_main.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h externs.h
pl_main.o: /usr/include/stdio.h /usr/include/signal.h /usr/include/ctype.h
pl_main.o: /usr/include/setjmp.h machdep.h /usr/include/sys/types.h
pl_main.o: /usr/include/sys/wait.h /usr/include/machine/machparam.h
pl_1.o: pl_1.c player.h /usr/include/curses.h /usr/include/stdio.h
pl_1.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
pl_1.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h externs.h
pl_1.o: /usr/include/stdio.h /usr/include/signal.h /usr/include/ctype.h
pl_1.o: /usr/include/setjmp.h machdep.h /usr/include/sys/types.h
pl_1.o: /usr/include/sys/wait.h /usr/include/machine/machparam.h
pl_2.o: pl_2.c player.h /usr/include/curses.h /usr/include/stdio.h
pl_2.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
pl_2.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h externs.h
pl_2.o: /usr/include/stdio.h /usr/include/signal.h /usr/include/ctype.h
pl_2.o: /usr/include/setjmp.h machdep.h
pl_3.o: pl_3.c player.h /usr/include/curses.h /usr/include/stdio.h
pl_3.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
pl_3.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h externs.h
pl_3.o: /usr/include/stdio.h /usr/include/signal.h /usr/include/ctype.h
pl_3.o: /usr/include/setjmp.h machdep.h
pl_4.o: pl_4.c player.h /usr/include/curses.h /usr/include/stdio.h
pl_4.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
pl_4.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h externs.h
pl_4.o: /usr/include/stdio.h /usr/include/signal.h /usr/include/ctype.h
pl_4.o: /usr/include/setjmp.h machdep.h
pl_5.o: pl_5.c player.h /usr/include/curses.h /usr/include/stdio.h
pl_5.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
pl_5.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h externs.h
pl_5.o: /usr/include/stdio.h /usr/include/signal.h /usr/include/ctype.h
pl_5.o: /usr/include/setjmp.h machdep.h
pl_6.o: pl_6.c player.h /usr/include/curses.h /usr/include/stdio.h
pl_6.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
pl_6.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h externs.h
pl_6.o: /usr/include/stdio.h /usr/include/signal.h /usr/include/ctype.h
pl_6.o: /usr/include/setjmp.h machdep.h
pl_7.o: pl_7.c player.h /usr/include/curses.h /usr/include/stdio.h
pl_7.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
pl_7.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h externs.h
pl_7.o: /usr/include/stdio.h /usr/include/signal.h /usr/include/ctype.h
pl_7.o: /usr/include/setjmp.h machdep.h
dr_main.o: dr_main.c driver.h externs.h /usr/include/stdio.h
dr_main.o: /usr/include/signal.h /usr/include/ctype.h /usr/include/setjmp.h
dr_main.o: machdep.h
dr_1.o: dr_1.c driver.h externs.h /usr/include/stdio.h /usr/include/signal.h
dr_1.o: /usr/include/ctype.h /usr/include/setjmp.h machdep.h
dr_2.o: dr_2.c driver.h externs.h /usr/include/stdio.h /usr/include/signal.h
dr_2.o: /usr/include/ctype.h /usr/include/setjmp.h machdep.h
dr_3.o: dr_3.c driver.h externs.h /usr/include/stdio.h /usr/include/signal.h
dr_3.o: /usr/include/ctype.h /usr/include/setjmp.h machdep.h
dr_4.o: dr_4.c externs.h /usr/include/stdio.h /usr/include/signal.h
dr_4.o: /usr/include/ctype.h /usr/include/setjmp.h machdep.h
dr_5.o: dr_5.c externs.h /usr/include/stdio.h /usr/include/signal.h
dr_5.o: /usr/include/ctype.h /usr/include/setjmp.h machdep.h
lo_main.o: lo_main.c /usr/include/pwd.h externs.h /usr/include/stdio.h
lo_main.o: /usr/include/signal.h /usr/include/ctype.h /usr/include/setjmp.h
lo_main.o: machdep.h
assorted.o: assorted.c externs.h /usr/include/stdio.h /usr/include/signal.h
assorted.o: /usr/include/ctype.h /usr/include/setjmp.h machdep.h
game.o: game.c externs.h /usr/include/stdio.h /usr/include/signal.h
game.o: /usr/include/ctype.h /usr/include/setjmp.h machdep.h
globals.o: globals.c externs.h /usr/include/stdio.h /usr/include/signal.h
globals.o: /usr/include/ctype.h /usr/include/setjmp.h machdep.h
misc.o: misc.c externs.h /usr/include/stdio.h /usr/include/signal.h
misc.o: /usr/include/ctype.h /usr/include/setjmp.h machdep.h
misc.o: /usr/include/sys/file.h
parties.o: parties.c externs.h /usr/include/stdio.h /usr/include/signal.h
parties.o: /usr/include/ctype.h /usr/include/setjmp.h machdep.h
sync.o: sync.c externs.h /usr/include/stdio.h /usr/include/signal.h
sync.o: /usr/include/ctype.h /usr/include/setjmp.h machdep.h
sync.o: /usr/include/sys/file.h /usr/include/sys/errno.h
sync.o: /usr/include/sys/types.h /usr/include/sys/stat.h
version.o: version.c

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
