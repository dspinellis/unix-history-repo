#	Makefile	4.1	83/07/03
#
DESTDIR=

all:
	cd src; make ${MFLAGS}

install: all
	cd src; make ${MFLAGS} DESTDIR=${DESTDIR} install
	cd man; make ${MFLAGS} DESTDIR=${DESTDIR} install

clean:
	cd src; make ${MFLAGS} clean
