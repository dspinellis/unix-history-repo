#
# Copyright (c) 1988 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that this notice is preserved and that due credit is given
# to the University of California at Berkeley. The name of the University
# may not be used to endorse or promote products derived from this
# software without specific prior written permission. This software
# is provided ``as is'' without express or implied warranty.
#
#	@(#)Makefile	5.1 (Berkeley) %G%
#
DESTDIR= 

all: FRC
	cd adb.${MACHINE}; make ${MFLAGS}

depend: FRC
	cd adb.${MACHINE}; make ${MFLAGS} depend

install: FRC
	cd adb.${MACHINE}; make DESTDIR=${DESTDIR} install
	cd scripts; make DESTDIR=${DESTDIR} install
	cd scripts.${MACHINE}; make DESTDIR=${DESTDIR} install

clean: FRC
	cd adb.${MACHINE}; make ${MFLAGS} clean

FRC:
