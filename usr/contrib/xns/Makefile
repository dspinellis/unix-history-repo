# INSTALL the following in the order specified
SUBDIRS=xnslib compiler courierlib morexnslib daemon examples
RCSNAME=current
DESTDIR=
CRLIB=${DESTDIR}/usr/new/lib/xnscourier
BINDIR=${DESTDIR}/usr/new

# remake everything (many things assume previous subdirectories in the
# list have had a "make install" done).
install:
	@echo You should edit morexnslib/ch/clearinghouse.addresses \
		before running this
	-rm -r -f ${DESTDIR}/usr/local/lib/libcourier.a \
		${DESTDIR}/usr/local/lib/libcourier_p.a
	for Dir in ${SUBDIRS}; do \
		(cd $$Dir; \
		make ${MFLAGS} DESTDIR=${DESTDIR} CRLIB=${CRLIB} \
			BINDIR=${BINDIR} install); done

all:
	for Dir in ${SUBDIRS}; do \
		(cd $$Dir; \
		make ${MFLAGS} DESTDIR=${DESTDIR} CRLIB=${CRLIB} \
			BINDIR=${BINDIR}); done

clean:
	-rm *.BAK *.CKP
	for Dir in ${SUBDIRS}; do (cd $$Dir; make ${MFLAGS} clean); done

rcs:
	for Dir in ${SUBDIRS}; do \
		(cd $$Dir; make RCSNAME=${RCSNAME} ${MFLAGS} rcs); done

# you should probably do a "make clean" and "make rcs" before this.
# also, edit rcsdescription
courier.tar: rcsdescription
	-chmod -r */RCS morexnslib/*/RCS examples/*/RCS
	tar cvf courier.tar Makefile README ${SUBDIRS}
	-chmod +r */RCS morexnslib/*/RCS examples/*/RCS

# courierbin.tar is a binary-only distribution of XNS Courier system
courierbin.tar:
	cd examples; make clean
	tar cvf courierbin.tar README-BINDIST \
		/usr/new/xnscourier \
		/usr/local/lib/libcourier.a \
		/usr/local/lib/libcourier_p.a \
		/usr/local/lib/clearinghouse.addresses \
		/usr/local/lib/courier \
		/usr/include/courier /usr/include/except.h \
		/etc/xnscourierd \
		doc examples

# in addition, /usr/local/{xnsprint,gaptelnet} etc.
