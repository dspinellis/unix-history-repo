#
# Copyright (c) 1989 The Regents of the University of California.
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
#	@(#)Makefile	5.6 (Berkeley) %G%
#
# reorder gives an editor command for most common terminals
# (in reverse order from n'th to 1'st most commonly used)
# to move them to the front of termcap
#
MAN=	termcap.0

all: termcap

termcap: reorder termcap.src
	ex - termcap.src < reorder

clean:
	rm -f termcap

cleandir:
	rm -f ${MAN} tags .depend

install: ${MAN}
	install -c -o bin -g bin -m 444 map3270 ${DESTDIR}/etc/map3270
	install -o bin -g staff -m 444 termcap ${DESTDIR}/etc/termcap
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat5
	rm -rf ${DESTDIR}/usr/lib/tabset
	mkdir ${DESTDIR}/usr/lib/tabset
	chown bin.bin ${DESTDIR}/usr/lib/tabset
	chmod 755 ${DESTDIR}/usr/lib/tabset
	install -c -o bin -g bin -m 444 tabset/* ${DESTDIR}/usr/lib/tabset

depend:
lint:
tags:
