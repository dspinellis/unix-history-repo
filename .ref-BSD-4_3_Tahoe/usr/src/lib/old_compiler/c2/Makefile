#
#	%M%	%I%	%E%
#
DESTDIR= 

all:	FRC
	cd c2.${MACHINE}; make ${MFLAGS}

clean: FRC
	cd c2.${MACHINE}; make ${MFLAGS} clean

depend: FRC
	cd c2.${MACHINE}; make ${MFLAGS} depend

install: FRC
	cd c2.${MACHINE}; make DESTDIR=${DESTDIR} install

FRC:
