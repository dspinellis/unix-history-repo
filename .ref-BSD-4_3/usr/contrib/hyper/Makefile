#	Makefile	4.3	85/09/07
#
DESTDIR=

all:	
	cd hyroute; make ${MFLAGS}
	cd hylog; make ${MFLAGS}

install: all
	cd hyroute; make ${MFLAGS} DESTDIR=${DESTDIR} install
	cd hylog; make ${MFLAGS} DESTDIR=${DESTDIR} install
	cd man; make ${MFLAGS} DESTDIR=${DESTDIR} install

clean:
	cd hyroute; make ${MFLAGS} clean
	cd hylog; make ${MFLAGS} clean
