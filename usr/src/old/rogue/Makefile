#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.1 (Berkeley) %G%
#
OBJS=		main.o machdep.o

SCOREFILE=	/usr/games/lib/rogue_roll
DEFS=		-DSCOREFILE='"$(SCOREFILE)"'
CFLAGS=		-O $(DEFS)
DESTDIR=	/usr/games

NROFF=	nroff
TROFF=	ditroff

rogue:	$(OBJS)
	$(CC) $(CFLAGS) -o rogue $(OBJS) -lcurses -ltermlib

doc.nroff: rogue.doc
	tbl rogue.doc | $(NROFF) -me -Tcrt > doc.nroff

doc.troff: rogue.doc
	tbl rogue.doc | $(TROFF) -me

all:	rogue doc.nroff doc.troff

install: rogue
	install -s -o daemon -m 4711 rogue $(DESTDIR)
	install -c -o daemon -m 644 /dev/null $(SCOREFILE)
