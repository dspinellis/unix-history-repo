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
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	4.14 (Berkeley) %G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
ALL=	libf77plot libplot lib300 lib300s lib4013 lib4014 lib450 libvt0 \
	libplotaed libplotbg libplotdumb libplotgigi libplot2648 libplot7221 \
	libplotimagen libplotgrn
SUBDIRS=tf77 plot t4013 t4014 t300 t300s t450 vt0 aed bitgraph dumb gigi \
	hp2648 hp7221 imagen grn
MAN=	plot.0
MDIR=	${DESTDIR}/usr/man/cat3

all: ${ALL}

libf77plot: FRC
	cd tf77; make ${MFLAGS}

libplot: FRC
	cd plot; make ${MFLAGS}

lib4013: FRC
	cd t4013; make ${MFLAGS}

lib4014: FRC
	cd t4014; make ${MFLAGS}

lib300: FRC
	cd t300; make ${MFLAGS}

lib300s: FRC
	cd t300s; make ${MFLAGS}

lib450: FRC
	cd t450; make ${MFLAGS}

libvt0: FRC
	cd vt0; make ${MFLAGS}

libplotaed: FRC
	cd aed; make ${MFLAGS}

libplotbg: FRC
	cd bitgraph; make ${MFLAGS}

libplotdumb: FRC
	cd dumb; make ${MFLAGS}

libplotgigi: FRC
	cd gigi; make ${MFLAGS}

libplot2648: FRC
	cd hp2648; make ${MFLAGS}

libplot7221: FRC
	cd hp7221; make ${MFLAGS}

libplotimagen: FRC
	cd imagen; make ${MFLAGS}

libplotgrn: FRC
	cd grn; make ${MFLAGS}

clean:
	rm -f ${ALL} core
	for i in ${SUBDIRS}; do \
		(cd $$i; make ${MFLAGS} clean); \
	done

cleandir: clean
	rm -f ${MAN} tags .depend

depend lint tags: FRC
	for i in ${SUBDIRS}; do \
		(cd $$i; make ${MFLAGS} $@); done

install: ${MAN}
	for i in ${ALL}; do \
		install -o bin -g bin -m 644 $$i ${DESTDIR}/usr/lib/$$i.a; \
		ranlib ${DESTDIR}/usr/lib/$$i.a; \
	done
	install -c -o bin -g bin -m 444 plot.0 ${DESTDIR}/usr/man/cat3
	rm -f ${MDIR}/arc.0; ln ${MDIR}/plot.0 ${MDIR}/arc.0
	rm -f ${MDIR}/circle.0; ln ${MDIR}/plot.0 ${MDIR}/circle.0
	rm -f ${MDIR}/cont.0; ln ${MDIR}/plot.0 ${MDIR}/cont.0
	rm -f ${MDIR}/erase.0; ln ${MDIR}/plot.0 ${MDIR}/erase.0
	rm -f ${MDIR}/label.0; ln ${MDIR}/plot.0 ${MDIR}/label.0
	rm -f ${MDIR}/line.0; ln ${MDIR}/plot.0 ${MDIR}/line.0
	rm -f ${MDIR}/linemod.0; ln ${MDIR}/plot.0 ${MDIR}/linemod.0
	rm -f ${MDIR}/move.0; ln ${MDIR}/plot.0 ${MDIR}/move.0
	rm -f ${MDIR}/openpl.0; ln ${MDIR}/plot.0 ${MDIR}/openpl.0
	rm -f ${MDIR}/point.0; ln ${MDIR}/plot.0 ${MDIR}/point.0
	rm -f ${MDIR}/space.0; ln ${MDIR}/plot.0 ${MDIR}/space.0

FRC:
