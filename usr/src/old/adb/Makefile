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
#	@(#)Makefile	5.2 (Berkeley) %G%
#
SUBDIR=	scripts scripts.${MACHINE} adb.${MACHINE}

all: FRC
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS}); done

depend: FRC
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} depend); done

SCRIPTDIR=	/usr/lib/adb
install: FRC
	rm -rf ${SCRIPTDIR}
	mkdir ${DESTDIR}/${SCRIPTDIR}
	chown bin.bin ${DESTDIR}/${SCRIPTDIR}
	chmod 755 ${DESTDIR}/${SCRIPTDIR}
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} install); done

clean: FRC
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} clean); done

FRC:
