#	Makefile	4.7	83/11/11
#
ALL=	libplot lib300 lib300s lib4014 lib450 libvt0 \
	libplotaed libplotbg libplotdumb libplotgigi libplot2648 libplot7221
SUBDIRS=plot t4014 t300 t300s t450 vt0 \
	aed bitgraph dumb gigi hp2648 hp7221

all:	${ALL}

libplot: FRC
	cd plot; make ${MFLAGS}

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

FRC:

install: all
	for i in ${ALL}; do \
		install -m 644 $$i ${DESTDIR}/usr/lib/$$i.a; \
		ranlib ${DESTDIR}/usr/lib/$$i.a; \
	done

clean:
	rm -f ${ALL} errs a.out core
	for i in ${SUBDIRS}; do \
		(cd $$i; make ${MFLAGS} clean); \
	done
