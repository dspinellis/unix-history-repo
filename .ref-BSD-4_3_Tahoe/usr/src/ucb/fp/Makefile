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
#	@(#)Makefile	5.1 (Berkeley) 12/23/87
#
all: FRC
	cd fp.${MACHINE}; make ${MFLAGS}

install: FRC
	cd fp.${MACHINE}; make DESTDIR=${DESTDIR} install

clean: FRC
	cd fp.${MACHINE}; make ${MFLAGS} clean

depend: FRC
	cd fp.${MACHINE}; make ${MFLAGS} depend

FRC:

