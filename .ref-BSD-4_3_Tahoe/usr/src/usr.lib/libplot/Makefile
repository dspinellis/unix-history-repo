#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.12	(Berkeley)	6/19/87
#
CFLAGS=	-O
LIBC=	/lib/libc.a
ALL=	libf77plot libplot lib300 lib300s lib4013 lib4014 lib450 libvt0 \
	libplotaed libplotbg libplotdumb libplotgigi libplot2648 libplot7221 \
	libplotimagen libplotgrn
SUBDIRS=tf77 plot t4013 t4014 t300 t300s t450 vt0 aed bitgraph dumb gigi \
	hp2648 hp7221 imagen grn

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

clean: FRC
	rm -f ${ALL} core
	for i in ${SUBDIRS}; do \
		(cd $$i; make ${MFLAGS} clean); \
	done

depend: FRC
	for i in ${SUBDIRS}; do \
		(cd $$i; make ${MFLAGS} depend); \
	done

install: FRC
	for i in ${ALL}; do \
		install -o bin -g bin -m 644 $$i ${DESTDIR}/usr/lib/$$i.a; \
		ranlib ${DESTDIR}/usr/lib/$$i.a; \
	done

tags: FRC
	for i in ${SUBDIRS}; do \
		(cd $$i; make ${MFLAGS} tags); \
	done

lint:
FRC:
