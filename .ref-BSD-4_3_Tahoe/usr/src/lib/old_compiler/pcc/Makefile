#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	%W%	(Berkeley)	%G%
#
DESTDIR= 

all:	FRC
	cd pcc.${MACHINE}; make ${MFLAGS}

clean: FRC
	cd pcc.${MACHINE}; make ${MFLAGS} clean

depend: FRC
	cd pcc.${MACHINE}; make ${MFLAGS} depend

install: FRC
	cd pcc.${MACHINE}; make DESTDIR=${DESTDIR} install

tags: FRC
	cd pcc.${MACHINE}; make ${MFLAGS} tags

FRC:
