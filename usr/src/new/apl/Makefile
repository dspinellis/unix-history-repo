#	Makefile	4.3	87/06/16
#
DESTDIR=

all: FRC
	cd apl.${MACHINE}; make ${MFLAGS}

install: FRC
	cd apl.${MACHINE}; make ${MFLAGS} DESTDIR=${DESTDIR} install

clean: FRC
	cd apl.${MACHINE}; make ${MFLAGS} clean

depend: FRC
	cd apl.${MACHINE}; make ${MFLAGS} depend

FRC:
