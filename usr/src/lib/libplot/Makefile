#	Makefile	4.4	83/07/09
#
ALL=	libplot libt4014 libt300 libt300s libt450 libvt0

all:	${ALL}

libplot: FRC
	cd plot; make ${MFLAGS}

libt4014: FRC
	cd t4014; make ${MFLAGS}

libt300: FRC
	cd t300; make ${MFLAGS}

libt300s: FRC
	cd t300s; make ${MFLAGS}

libt450: FRC
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
	for i in ${ALL}; do \
		(cd $$i; make ${MFLAGS} clean); \
	done
