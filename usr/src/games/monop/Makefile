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
#	@(#)Makefile	5.2 (Berkeley) %G%
#
# Nifty foo monopoly maker
#	Program:  Ken Arnold
#	Makefile:  K.S.
#
CFLAGS=	-O
LIBC=	/lib/libc.a
MSRCS=	monop.c cards.c execute.c getinp.c houses.c jail.c misc.c morg.c \
	print.c prop.c rent.c roll.c spec.c trade.c
MOBJS=	monop.o cards.o execute.o getinp.o houses.o jail.o misc.o morg.o \
	print.o prop.o rent.o roll.o spec.o trade.o
ISRCS=	initdeck.c
IOBJS=	initdeck.o

all: monop initdeck cards.pck

monop:	${MOBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${MOBJS}

initdeck: ${IOBJS}
	${CC} ${CFLAGS} -o $@ ${IOBJS}

cards.pck: FRC
	./initdeck

clean: FRC
	rm -f ${IOBJS} ${MOBJS} core monop initdeck cards.pck

depend: FRC
	mkdep ${CFLAGS} ${MSRCS} ${ISRCS}

install: FRC
	install -o games -g bin -m 400 cards.pck ${DESTDIR}/usr/games/lib
	install -s -o games -g bin -m 700 monop ${DESTDIR}/usr/games/hide
	(cd ${DESTDIR}/usr/games; rm -f monop; ln -s dm monop; chown games.bin monop)

lint: FRC
	lint ${CFLAGS} ${MSRCS}
	lint ${CFLAGS} ${ISRCS}

tags: FRC
	ctags ${MSRCS} ${ISRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

monop.o: monop.c monop.def monop.h /usr/include/stdio.h deck.h mon.dat prop.dat
monop.o: brd.dat
cards.o: cards.c monop.ext monop.h /usr/include/stdio.h deck.h
execute.o: execute.c monop.ext monop.h /usr/include/stdio.h deck.h
execute.o: /usr/include/sys/types.h /usr/include/sys/stat.h
execute.o: /usr/include/sys/time.h /usr/include/time.h
getinp.o: getinp.c /usr/include/stdio.h /usr/include/ctype.h
houses.o: houses.c monop.ext monop.h /usr/include/stdio.h deck.h
jail.o: jail.c monop.ext monop.h /usr/include/stdio.h deck.h
misc.o: misc.c monop.ext monop.h /usr/include/stdio.h deck.h
misc.o: /usr/include/ctype.h /usr/include/signal.h /usr/include/machine/trap.h
misc.o: /usr/include/pwd.h
morg.o: morg.c monop.ext monop.h /usr/include/stdio.h deck.h
print.o: print.c monop.ext monop.h /usr/include/stdio.h deck.h
prop.o: prop.c monop.ext monop.h /usr/include/stdio.h deck.h
rent.o: rent.c monop.ext monop.h /usr/include/stdio.h deck.h
roll.o: roll.c
spec.o: spec.c monop.ext monop.h /usr/include/stdio.h deck.h
trade.o: trade.c monop.ext monop.h /usr/include/stdio.h deck.h
initdeck.o: initdeck.c /usr/include/stdio.h deck.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
