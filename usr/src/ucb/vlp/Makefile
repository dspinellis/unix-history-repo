#
#	%M%	%I%	%E%
#
DESTDIR= 

all:	FRC
	cd vlp.${MACHINE}; make ${MFLAGS}

FRC:

install:
	cd vlp.${MACHINE}; make DESTDIR=${DESTDIR} install

clean:
	cd vlp.${MACHINE}; make ${MFLAGS} clean

depend:
	cd vlp.${MACHINE}; make ${MFLAGS} depend
