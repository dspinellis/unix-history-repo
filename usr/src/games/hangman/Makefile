#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.2	(Berkeley)	%G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
LIBS=	-lcurses -ltermlib
SRCS=	endgame.c extern.c getguess.c getword.c main.c playgame.c \
	prdata.c prman.c prword.c setup.c
OBJS=	endgame.o extern.o getguess.o getword.o main.o playgame.o \
	prdata.o prman.o prword.o setup.o

all: hangman

hangman: ${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS} ${LIBS}

clean: FRC
	rm -f ${OBJS} core hangman

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 hangman ${DESTDIR}/etc/hangman

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

endgame.o: endgame.c hangman.h /usr/include/curses.h /usr/include/stdio.h
endgame.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
endgame.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
endgame.o: /usr/include/sys/types.h /usr/include/sys/stat.h
endgame.o: /usr/include/ctype.h /usr/include/signal.h
extern.o: extern.c hangman.h /usr/include/curses.h /usr/include/stdio.h
extern.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
extern.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
extern.o: /usr/include/sys/types.h /usr/include/sys/stat.h /usr/include/ctype.h
extern.o: /usr/include/signal.h
getguess.o: getguess.c hangman.h /usr/include/curses.h /usr/include/stdio.h
getguess.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
getguess.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
getguess.o: /usr/include/sys/types.h /usr/include/sys/stat.h
getguess.o: /usr/include/ctype.h /usr/include/signal.h
getword.o: getword.c hangman.h /usr/include/curses.h /usr/include/stdio.h
getword.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
getword.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
getword.o: /usr/include/sys/types.h /usr/include/sys/stat.h
getword.o: /usr/include/ctype.h /usr/include/signal.h
main.o: main.c hangman.h /usr/include/curses.h /usr/include/stdio.h
main.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
main.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
main.o: /usr/include/sys/types.h /usr/include/sys/stat.h /usr/include/ctype.h
main.o: /usr/include/signal.h
playgame.o: playgame.c hangman.h /usr/include/curses.h /usr/include/stdio.h
playgame.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
playgame.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
playgame.o: /usr/include/sys/types.h /usr/include/sys/stat.h
playgame.o: /usr/include/ctype.h /usr/include/signal.h
prdata.o: prdata.c hangman.h /usr/include/curses.h /usr/include/stdio.h
prdata.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
prdata.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
prdata.o: /usr/include/sys/types.h /usr/include/sys/stat.h /usr/include/ctype.h
prdata.o: /usr/include/signal.h
prman.o: prman.c hangman.h /usr/include/curses.h /usr/include/stdio.h
prman.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
prman.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
prman.o: /usr/include/sys/types.h /usr/include/sys/stat.h /usr/include/ctype.h
prman.o: /usr/include/signal.h
prword.o: prword.c hangman.h /usr/include/curses.h /usr/include/stdio.h
prword.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
prword.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
prword.o: /usr/include/sys/types.h /usr/include/sys/stat.h /usr/include/ctype.h
prword.o: /usr/include/signal.h
setup.o: setup.c hangman.h /usr/include/curses.h /usr/include/stdio.h
setup.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
setup.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
setup.o: /usr/include/sys/types.h /usr/include/sys/stat.h /usr/include/ctype.h
setup.o: /usr/include/signal.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
