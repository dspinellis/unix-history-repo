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
#	@(#)Makefile	5.1 (Berkeley) %G%
#
MDIR=	/usr/man/cat8/vax
SRCS=	arff.8 bad144.8 crash.8 drtest.8 format.8 installboot.8 rxformat.8 \
	reboot.8
OBJS=	arff.0 bad144.0 crash.0 drtest.0 format.0 installboot.0 rxformat.0 \
	reboot.0
LINKS=	flcopy.0

.SUFFIXES: .8 .0

.8.0:
	${DESTDIR}/usr/man/manroff $*.8 > $*.0

all: ${OBJS}

clean: FRC
	rm -f ${OBJS}

install: FRC
	cd ${DESTDIR}${MDIR}; rm -f *.0
	install -c -o bin -g bin -m 444 ${OBJS} ${DESTDIR}${MDIR}
	ln ${DESTDIR}${MDIR}/arff.0 ${DESTDIR}${MDIR}/flcopy.0

FRC:
