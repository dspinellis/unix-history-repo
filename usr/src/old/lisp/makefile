#
# Copyright (c) 1987, 1988 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that this notice is preserved and that due credit is given
# to the University of California at Berkeley. The name of the University
# may not be used to endorse or promote products derived from this
# software without specific prior written permission. This software
# is provided ``as is'' without express or implied warranty.
#
#	@(#)makefile	5.5 (Berkeley) %G%
#

all: FRC
	./lispconf ${MACHINE}_4_3
	make -f Makefile ${MFLAGS} DESTDIR=${DESTDIR} copylibrary
	make -f Makefile ${MFLAGS}

install: FRC
	make -f Makefile ${MFLAGS} DESTDIR=${DESTDIR} install

clean: FRC
	make -f Makefile ${MFLAGS} clean

depend: FRC
	make -f Makefile ${MFLAGS} depend

FRC:
