#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.2	(Berkeley)	6/3/87
#
all:

install: FRC
	install -c -o bin -g bin -m 755 ching.sh ${DESTDIR}/usr/games/ching
	-mkdir ${DESTDIR}/usr/games/lib/ching.d > /dev/null 2>&1
	install -c -o bin -g bin -m 755 cno ${DESTDIR}/usr/games/lib/ching.d/cno
	install -c -o bin -g bin -m 755 phx ${DESTDIR}/usr/games/lib/ching.d/phx
	install -c -o bin -g bin -m 444 macros ${DESTDIR}/usr/games/lib/ching.d/macros
	install -c -o bin -g bin -m 444 hexagrams ${DESTDIR}/usr/games/lib/ching.d/hexagrams

clean:
depend:
lint:
tags:
FRC:
