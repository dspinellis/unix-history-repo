#	%M%	%I%	%E%
#
DESTDIR=

all: FRC
	cd src; make ${MFLAGS}

install: FRC
	cd src; make ${MFLAGS} DESTDIR=${DESTDIR} install
	cd lib; make ${MFLAGS} DESTDIR=${DESTDIR} install
	cd man; make ${MFLAGS} DESTDIR=${DESTDIR} install

clean: FRC
	cd src; make ${MFLAGS} clean

depend:
FRC:
