#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	1.9	(Berkeley)	%G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
LIBS=	-lcurses -ltermlib
OBJS=	battlestar.o com1.o com2.o com3.o com4.o com5.o com6.o com7.o \
	init.o cypher.o getcom.o parse.o room.o save.o fly.o misc.o \
	globals.o dayfile.o nightfile.o dayobjs.o nightobjs.o words.o
SRCS=	battlestar.c com1.c com2.c com3.c com4.c com5.c com6.c com7.c \
	init.c cypher.c getcom.c parse.c room.c save.c fly.c misc.c \
	globals.c dayfile.c nightfile.c dayobjs.c nightobjs.c words.c

all: battlestar

battlestar: ${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS} ${LIBS}

clean: FRC
	rm -f ${OBJS} core battlestar

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 battlestar ${DESTDIR}/usr/games/battlestar
	install -c -o bin -g bin -m 666 /dev/null ${DESTDIR}/usr/games/lib/battlestar.log

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

battlestar.o: battlestar.c externs.h /usr/include/sys/signal.h
battlestar.o: /usr/include/stdio.h
com1.o: com1.c externs.h /usr/include/sys/signal.h /usr/include/stdio.h
com2.o: com2.c externs.h /usr/include/sys/signal.h /usr/include/stdio.h
com3.o: com3.c externs.h /usr/include/sys/signal.h /usr/include/stdio.h
com4.o: com4.c externs.h /usr/include/sys/signal.h /usr/include/stdio.h
com5.o: com5.c externs.h /usr/include/sys/signal.h /usr/include/stdio.h
com6.o: com6.c externs.h /usr/include/sys/signal.h /usr/include/stdio.h
com6.o: /usr/include/sys/time.h /usr/include/time.h
com7.o: com7.c externs.h /usr/include/sys/signal.h /usr/include/stdio.h
init.o: init.c externs.h /usr/include/sys/signal.h /usr/include/stdio.h
init.o: /usr/include/pwd.h
cypher.o: cypher.c externs.h /usr/include/sys/signal.h /usr/include/stdio.h
getcom.o: getcom.c /usr/include/stdio.h /usr/include/ctype.h
parse.o: parse.c externs.h /usr/include/sys/signal.h /usr/include/stdio.h
room.o: room.c externs.h /usr/include/sys/signal.h /usr/include/stdio.h
save.o: save.c externs.h /usr/include/sys/signal.h /usr/include/stdio.h
fly.o: fly.c externs.h /usr/include/sys/signal.h /usr/include/stdio.h
fly.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
fly.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
fly.o: /usr/include/sys/ttydev.h
misc.o: misc.c externs.h /usr/include/sys/signal.h /usr/include/stdio.h
globals.o: globals.c externs.h /usr/include/sys/signal.h /usr/include/stdio.h
dayfile.o: dayfile.c externs.h /usr/include/sys/signal.h /usr/include/stdio.h
nightfile.o: nightfile.c externs.h /usr/include/sys/signal.h
nightfile.o: /usr/include/stdio.h
dayobjs.o: dayobjs.c externs.h /usr/include/sys/signal.h /usr/include/stdio.h
nightobjs.o: nightobjs.c externs.h /usr/include/sys/signal.h
nightobjs.o: /usr/include/stdio.h
words.o: words.c externs.h /usr/include/sys/signal.h /usr/include/stdio.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
