#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.3	(Berkeley)	%G%
#

all: aard

aard: FRC
	../ddl/ddlcomp aard < aard.ddl > ddlcomp.out

clean: FRC
	rm -f ddlcomp.out aard

install: FRC
	install -o bin -g bin -m 755 aard ${DESTDIR}/usr/games/lib/aard
	install -c -o bin -g bin -m 755 frontend ${DESTDIR}/usr/games/aardvark

depend:
lint:
tags:
FRC:
