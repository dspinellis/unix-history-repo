#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that this notice is preserved and that due credit is given
# to the University of California at Berkeley. The name of the University
# may not be used to endorse or promote products derived from this
# software without specific prior written permission. This software
# is provided ``as is'' without express or implied warranty.
#
#	@(#)makefile	5.1 (Berkeley) %G%
#

all: FRC
	cd lisp; ./lispconf ${MACHINE}_4_3
	cd lisp; make DESTDIR=${DESTDIR} copylibrary
	cd lisp; make ${MFLAGS}

install: FRC
	cd lisp; make DESTDIR=${DESTDIR} install

clean: FRC
	cd lisp; make ${MFLAGS} clean

depend: FRC
	cd lisp; make ${MFLAGS} depend

FRC:
