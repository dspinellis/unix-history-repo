#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.4 (Berkeley) 6/3/87
#
OBJS=		main.obj machdep.o

GAMESDIR=	$(DESTDIR)/usr/games
SCOREFILE=	$(DESTDIR)/usr/games/lib/rogue_roll
DEFS=		-DSCOREFILE='"$(SCOREFILE)"'
CFLAGS=		-O $(DEFS)

NROFF=	nroff
TROFF=	ditroff

rogue:	$(OBJS)
	$(CC) $(CFLAGS) -o rogue $(OBJS) -lcurses -ltermlib

install: rogue
	install -s -o daemon -m 4711 rogue $(GAMESDIR)
	install -c -o daemon -m 644 /dev/null $(SCOREFILE)

clean:
	rm -f rogue machdep.o

depend:
