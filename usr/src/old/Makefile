# Kludge Makefile to make just uncompact and old talk.

DESTDIR=

all: FRC
	cd compact; make ${MFLAGS} uncompact
	cd talk; make ${MFLAGS}

install: FRC
	cd compact; install -s uncompact ${DESTDIR}/usr/old
	cd talk; make ${MFLAGS} DESTDIR=$(DESTDIR) install

clean: FRC
	cd compact; make ${MFLAGS} clean
	cd talk; make ${MFLAGS} clean

FRC:

