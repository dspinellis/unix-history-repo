#
#	%M%	%I%	%E%
#
DESTDIR= 

all:	FRC
	cd as.${MACHINE}; make ${MFLAGS}

install: FRC
	cd as.${MACHINE}; make DESTDIR=${DESTDIR} install

clean: FRC
	cd as.${MACHINE}; make ${MFLAGS} clean

depend: FRC
	cd as.${MACHINE}; make ${MFLAGS} depend

FRC:
