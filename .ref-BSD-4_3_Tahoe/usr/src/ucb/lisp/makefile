#
# Copyright (c) 1987, 1988 Regents of the University of California.
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
#	@(#)makefile	5.6 (Berkeley) 7/9/88
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
