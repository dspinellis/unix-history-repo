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
LIST=	findproc.nxt frame pcb proc setproc.nxt trapframe tty

all:

install: FRC
	install -c -o bin -g bin -m 444 ${LIST} ${DESTDIR}/usr/lib/adb

clean:
depend:
lint:
tags:
FRC:

# DO NOT DELETE THIS LINE -- make depend uses it

