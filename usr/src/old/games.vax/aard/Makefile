#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.1 (Berkeley) %G%
#
DESTDIR=

aard:
	../ddl/ddlcomp aard < aard.ddl > ddlcomp.out

install:
	install aard ${DESTDIR}/usr/games/lib/aard
	install -c -m 755 frontend ${DESTDIR}/usr/games/aardvark

clean:
	rm -f ddlcomp.out
