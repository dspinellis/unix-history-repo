#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.2 (Berkeley) %G%
#
OBJS=		main.obj machdep.o

SCOREFILE=	/usr/games/lib/rogue_roll
DEFS=		-DSCOREFILE='"$(SCOREFILE)"'
CFLAGS=		-O $(DEFS)
DESTDIR=	/usr/games

NROFF=	nroff
TROFF=	ditroff

rogue:	$(OBJS)
	$(CC) $(CFLAGS) -o rogue $(OBJS) -lcurses -ltermlib

install: rogue
	install -s -o daemon -m 4711 rogue $(DESTDIR)
	install -c -o daemon -m 644 /dev/null $(SCOREFILE)

clean:
	rm -f rogue machdep.o
