#	Makefile	4.1	83/06/27
#
all: libplot.a libt4014.a libt300.a libt300s.a libt450.a libvt0.a

libplot.a: FRC
	cd plot; make ${MFLAGS}

libt4014.a: FRC
	cd t4014; make ${MFLAGS}

libt300.a: FRC
	cd t300; make ${MFLAGS}

libt300s.a: FRC
	cd t300s; make ${MFLAGS}

libt450.a: FRC
	cd t450; make ${MFLAGS}

libvt0.a: FRC
	cd vt0; make ${MFLAGS}

FRC:

install: all
	for i in ${ALL}; do \
		install -m 644 $$i ${DESTDIR}/usr/lib/$$i; \
		ranlib ${DESTDIR}/usr/lib/$$i; \
	done

clean:
	rm -f ${ALL} errs a.out core
