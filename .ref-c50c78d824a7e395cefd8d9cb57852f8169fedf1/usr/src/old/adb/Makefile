#
# Copyright (c) 1988 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation,
# advertising materials, and other materials related to such
# distribution and use acknowledge that the software was developed
# by the University of California, Berkeley.  The name of the
# University may not be used to endorse or promote products derived
# from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	5.4 (Berkeley) %G%
#
MAN=	adb.0

all depend clean lint tags: FRC
	cd adb.${MACHINE}; make ${MFLAGS} DESTDIR=${DESTDIR} $@

cleandir: clean
	rm -f ${MAN} tags .depend
	cd adb.${MACHINE}; make ${MFLAGS} DESTDIR=${DESTDIR} $@

SCRIPTDIR=	/usr/lib/adb

install: ${MAN}
	rm -rf ${DESTDIR}/${SCRIPTDIR}
	mkdir ${DESTDIR}/${SCRIPTDIR}
	chown bin.bin ${DESTDIR}/${SCRIPTDIR}
	chmod 755 ${DESTDIR}/${SCRIPTDIR}
	cd adb.${MACHINE}; make ${MFLAGS} DESTDIR=${DESTDIR} $@
	cd scripts; make ${MFLAGS} DESTDIR=${DESTDIR} $@
	cd scripts.${MACHINE}; make ${MFLAGS} DESTDIR=${DESTDIR} $@
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat1

FRC:
