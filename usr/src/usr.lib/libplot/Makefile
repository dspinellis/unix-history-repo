#	Makefile	4.6	83/09/21
#
ALL=	libplot lib4014 lib300 lib300s lib450 libvt0
SUBDIRS=plot t4014 t300 t300s t450 vt0

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
