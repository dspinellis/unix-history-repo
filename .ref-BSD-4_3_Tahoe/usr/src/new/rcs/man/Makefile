#
# Copyright (c) 1987 Regents of the University of California.
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
#	@(#)Makefile	1.5 (Berkeley) 7/9/88
#

SRCS=	ci.1 co.1 ident.1 rcs.1 rcsdiff.1 rcsfile.5 rcsfreeze.1 \
	rcsintro.1 rcsmerge.1 rlog.1 sccstorcs.1 merge.1
OBJS1=	ci.0 co.0 ident.0 rcs.0 rcsdiff.0 rcsfreeze.0 rcsintro.0 \
	rcsmerge.0 rlog.0 sccstorcs.0 merge.0
OBJS5=	rcsfile.0
OBJS=	${OBJS1} ${OBJS5}
LINKS=

.SUFFIXES: .5 .1 .0

.5.0:
	${DESTDIR}/usr/man/manroff $*.5 > $*.0
.1.0:
	${DESTDIR}/usr/man/manroff $*.1 > $*.0

all: ${OBJS}

clean: FRC
	rm -f ${OBJS}

install: FRC
	install -c -o bin -g bin -m 444 ${OBJS1} ${DESTDIR}/usr/new/man/cat1
	install -c -o bin -g bin -m 444 ${OBJS5} ${DESTDIR}/usr/new/man/cat5

FRC:

