#	@(#)Makefile	2.1 (Berkeley) 4/8/88
#
DESTDIR=

all:
	cd src; make ${MFLAGS}

install: all
	cd src; make ${MFLAGS} DESTDIR=${DESTDIR} install
	cd macs; make ${MFLAGS} DESTDIR=${DESTDIR} install
	cd man; make ${MFLAGS} DESTDIR=${DESTDIR} install

clean:
	cd src; make ${MFLAGS} clean
