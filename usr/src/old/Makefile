# Kludge Makefile to make just uncompact

DESTDIR=

all: FRC
	cd compact; make ${MFLAGS} uncompact

depend:

install: FRC
	cd compact; install -s uncompact ${DESTDIR}/usr/old

clean: FRC
	cd compact; make ${MFLAGS} clean

FRC:

