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
#	@(#)Makefile	5.2 (Berkeley) 6/19/88
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
	install -s -o games -g bin -m 4700 battlestar ${DESTDIR}/usr/games/hide/battlestar
	install -c -o games -g bin -m 600 /dev/null ${DESTDIR}/usr/games/lib/battlestar.log
	(cd ${DESTDIR}/usr/games; rm -f battlestar; ln -s dm battlestar; chown games.bin battlestar)

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

battlestar.o: battlestar.c externs.h /usr/include/sys/signal.h
battlestar.o: /usr/include/machine/trap.h /usr/include/stdio.h
com1.o: com1.c externs.h /usr/include/sys/signal.h /usr/include/machine/trap.h
com1.o: /usr/include/stdio.h
com2.o: com2.c externs.h /usr/include/sys/signal.h /usr/include/machine/trap.h
com2.o: /usr/include/stdio.h
com3.o: com3.c externs.h /usr/include/sys/signal.h /usr/include/machine/trap.h
com3.o: /usr/include/stdio.h
com4.o: com4.c externs.h /usr/include/sys/signal.h /usr/include/machine/trap.h
com4.o: /usr/include/stdio.h
com5.o: com5.c externs.h /usr/include/sys/signal.h /usr/include/machine/trap.h
com5.o: /usr/include/stdio.h
com6.o: com6.c externs.h /usr/include/sys/signal.h /usr/include/machine/trap.h
com6.o: /usr/include/stdio.h /usr/include/sys/time.h /usr/include/time.h
com7.o: com7.c externs.h /usr/include/sys/signal.h /usr/include/machine/trap.h
com7.o: /usr/include/stdio.h
init.o: init.c externs.h /usr/include/sys/signal.h /usr/include/machine/trap.h
init.o: /usr/include/stdio.h /usr/include/pwd.h
cypher.o: cypher.c externs.h /usr/include/sys/signal.h
cypher.o: /usr/include/machine/trap.h /usr/include/stdio.h
getcom.o: getcom.c /usr/include/stdio.h /usr/include/ctype.h
parse.o: parse.c externs.h /usr/include/sys/signal.h
parse.o: /usr/include/machine/trap.h /usr/include/stdio.h
room.o: room.c externs.h /usr/include/sys/signal.h /usr/include/machine/trap.h
room.o: /usr/include/stdio.h
save.o: save.c externs.h /usr/include/sys/signal.h /usr/include/machine/trap.h
save.o: /usr/include/stdio.h
fly.o: fly.c externs.h /usr/include/sys/signal.h /usr/include/machine/trap.h
fly.o: /usr/include/stdio.h /usr/include/curses.h /usr/include/stdio.h
fly.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
fly.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
misc.o: misc.c externs.h /usr/include/sys/signal.h /usr/include/machine/trap.h
misc.o: /usr/include/stdio.h
globals.o: globals.c externs.h /usr/include/sys/signal.h
globals.o: /usr/include/machine/trap.h /usr/include/stdio.h
dayfile.o: dayfile.c externs.h /usr/include/sys/signal.h
dayfile.o: /usr/include/machine/trap.h /usr/include/stdio.h
nightfile.o: nightfile.c externs.h /usr/include/sys/signal.h
nightfile.o: /usr/include/machine/trap.h /usr/include/stdio.h
dayobjs.o: dayobjs.c externs.h /usr/include/sys/signal.h
dayobjs.o: /usr/include/machine/trap.h /usr/include/stdio.h
nightobjs.o: nightobjs.c externs.h /usr/include/sys/signal.h
nightobjs.o: /usr/include/machine/trap.h /usr/include/stdio.h
words.o: words.c externs.h /usr/include/sys/signal.h
words.o: /usr/include/machine/trap.h /usr/include/stdio.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
