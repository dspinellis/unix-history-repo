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
#	@(#)Makefile	5.7 (Berkeley) 2/27/89
#

MAN1=	ci.0 co.0 ident.0 merge.0 rcs.0 rcsclean.0 rcsdiff.0 \
	rcsfreeze.0 rcsintro.0 rcsmerge.0 rlog.0 sccstorcs.0
MAN5=	rcsfile.0

all clean depend lint tags:

cleandir: clean
	rm -f ${MAN1} ${MAN5}

install: ${MAN1} ${MAN5}
	install -c -o bin -g bin -m 444 ${MAN1} ${DESTDIR}/usr/new/man/cat1
	install -c -o bin -g bin -m 444 ${MAN5} ${DESTDIR}/usr/new/man/cat5
