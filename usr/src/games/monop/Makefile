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
#	@(#)Makefile	5.3 (Berkeley) %G%
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
MAN=	monop.0

all: monop initdeck cards.pck

monop: ${MOBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${MOBJS}

initdeck: ${IOBJS}
	${CC} ${CFLAGS} -o $@ ${IOBJS}

cards.pck:
	./initdeck

clean:
	rm -f ${IOBJS} ${MOBJS} core monop initdeck cards.pck

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep ${CFLAGS} ${MSRCS} ${ISRCS}

install: ${MAN}
	install -o games -g bin -m 400 cards.pck ${DESTDIR}/usr/games/lib
	install -s -o games -g bin -m 700 monop ${DESTDIR}/usr/games/hide
	(cd ${DESTDIR}/usr/games; rm -f monop; ln -s dm monop; chown games.bin monop)
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat6

lint: ${SRCS}
	lint ${CFLAGS} ${MSRCS}
	lint ${CFLAGS} ${ISRCS}

tags: ${SRCS}
	ctags ${MSRCS} ${ISRCS}
