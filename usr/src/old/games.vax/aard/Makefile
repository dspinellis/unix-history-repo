#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.4	(Berkeley)	%G%
#

all: aard

aard: FRC
	../ddl/ddlcomp aard < aard.ddl > ddlcomp.out

clean: FRC
	rm -f ddlcomp.out aard

install: FRC
	install -o games -g bin -m 755 aard ${DESTDIR}/usr/games/lib/aard
	install -c -o games -g bin -m 755 frontend ${DESTDIR}/usr/games/hide/aardvark
	rm -f ${DESTDIR}/usr/games/aardvark
	ln -s ${DESTDIR}/usr/games/dm ${DESTDIR}/usr/games/aardvark
	chown games.bin ${DESTDIR}/usr/games/aardvark

depend:
lint:
tags:
FRC:
