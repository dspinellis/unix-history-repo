#
# Copyright (c) 1987 Regents of the University of California.
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
#	@(#)Makefile	5.2 (Berkeley) %G%
#
MDIR=	/usr/man/cat8/vax
SRCS=	arff.8 bad144.8 crash.8 drtest.8 format.8 installboot.8 rxformat.8 \
	reboot.8
OBJS=	arff.0 bad144.0 crash.0 drtest.0 format.0 installboot.0 rxformat.0 \
	reboot.0
LINKS=	flcopy.0

.SUFFIXES: .8 .0

.8.0:
	${DESTDIR}/usr/man/manroff $*.8 > $*.0

all: ${OBJS}

clean: FRC
	rm -f ${OBJS}

install: FRC
	cd ${DESTDIR}${MDIR}; rm -f *.0
	install -c -o bin -g bin -m 444 ${OBJS} ${DESTDIR}${MDIR}
	ln ${DESTDIR}${MDIR}/arff.0 ${DESTDIR}${MDIR}/flcopy.0

FRC:
