#
# Makefile for CHESS
#
# Copyright (C) 1986, 1987, 1988 Free Software Foundation, Inc.
#
# This file is part of CHESS.
#
# CHESS is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY.  No author or distributor
# accepts responsibility to anyone for the consequences of using it
# or for whether it serves any particular purpose or works at all,
# unless he says so in writing.  Refer to the CHESS General Public
# License for full details.
#
# Everyone is granted permission to copy, modify and redistribute
# CHESS, but only under the conditions described in the
# CHESS General Public License.   A copy of this license is
# supposed to have been given to you along with CHESS so you
# can know your rights and responsibilities.  It should be in a
# file named COPYING.  Among other things, the copyright notice
# and this notice must be preserved on all copies.
#
#
# gnuchess will be the alpha-display version (with curses/termcap)
# gnuchessr will be the non-display version (for dumb terminals and
#    for use with SUN's chesstool and X-window's xchess.
#    To use with latter, add -DCHESSTOOL to the gnuchess.o/nondsp.o
#    compilation lines (e.g. cc -O -DCHESSTOOL ...)
#
NEW=move.o
CFLAGS= -O -DNEWMOVE=12
all : gnuchess #gnuchessr
MAN=	chess.0

gnuchess: gnuchess.o uxdsp.o $(NEW)
	$(CC) $(CFLAGS) -o gnuchess gnuchess.o uxdsp.o $(NEW) \
		-lcurses -ltermlib -lcompat

gnuchessr: gnuchess.o nondsp.o $(NEW)
	$(CC) $(CFLAGS) -o gnuchessr gnuchess.o nondsp.o $(NEW) -lcompat

gnuchess.o: gnuchess.c
	$(CC) $(CFLAGS) -c gnuchess.c

move.o: move.c
	$(CC) $(CFLAGS) -c move.c

uxdsp.o: uxdsp.c
	$(CC) $(CFLAGS) -c uxdsp.c

nondsp.o: nondsp.c
	$(CC) $(CFLAGS) -c nondsp.c

distribution:
	tar cf - gnuchess.h gnuchess.c move.h move.c uxdsp.c nondsp.c Xchess gnuchess.book DOCUMENTATION Makefile > gnuchess.tar
	compress gnuchess.tar

install: ${MAN}
	install -s -o games -g bin -m 4700 gnuchess ${DESTDIR}/usr/games/hide/chess
	(cd ${DESTDIR}/usr/games; rm -f chess; ln -s dm chess; chown games.bin chess)
	install -c -o bin -g bin -m 444 gnuchess.book /usr/games/lib
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat6

#	cp gnuchessr /usr/games/gnuchess.chesstool
#	cp gnuchess /usr/games/gnuchess
#	cp gnuchess.book /usr/games/lib/gnuchess.book

clean:
	-rm gnuchess gnuchessr *.o

