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
#	@(#)Makefile	4.9 (Berkeley) 5/11/89
#
MAN=	doctor.0

all: doctor

doctor:
	liszt -mrq -o doctor doctor.l

install: ${MAN}
	install -o games -g bin -m 700 doctor ${DESTDIR}/usr/games/hide
	rm -f ${DESTDIR}/usr/games/doctor
	ln -s ${DESTDIR}/usr/games/dm ${DESTDIR}/usr/games/doctor
	chown games.bin ${DESTDIR}/usr/games/doctor
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat6

clean:
	rm -f doctor

cleandir: clean
	rm -f ${MAN}

depend lint tags:
