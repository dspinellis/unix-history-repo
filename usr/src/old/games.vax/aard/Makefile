#
# Copyright (c) 1987 The Regents of the University of California.
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
#	@(#)Makefile	5.6 (Berkeley) 10/19/88
#
MAN=	aardvark.0

all: aard

aard:
	-../ddl/ddlcomp aard < aard.ddl > ddlcomp.out

cleandir clean:
	rm -f ddlcomp.out aard

install: ${MAN}
	install -o games -g bin -m 755 aard ${DESTDIR}/usr/games/lib/aard
	install -c -o games -g bin -m 755 frontend ${DESTDIR}/usr/games/hide/aardvark
	rm -f ${DESTDIR}/usr/games/aardvark
	ln -s ${DESTDIR}/usr/games/dm ${DESTDIR}/usr/games/aardvark
	chown games.bin ${DESTDIR}/usr/games/aardvark
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat6/vax

depend lint tags:
