#
# Copyright (c) 1988 The Regents of the University of California.
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
#	@(#)Makefile	4.14 (Berkeley) %G%
#
SUBDIR=	lib src aux

all: ${SUBDIR}

${SUBDIR}: FRC
	cd $@; make ${MFLAGS}

clean cleandir depend: FRC
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} clean); done

install: FRC
	-mkdir ${DESTDIR}/usr/spool/mqueue
	chown root.wheel ${DESTDIR}/usr/spool/mqueue
	chmod 775 ${DESTDIR}/usr/spool/mqueue
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} install); done

