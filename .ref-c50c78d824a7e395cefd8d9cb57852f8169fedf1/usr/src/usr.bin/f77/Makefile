#
# Copyright (c) 1987 The Regents of the University of California.
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
#	@(#)Makefile	5.5 (Berkeley) %G%
#
MAN=	f77.0
SUBDIR=	f77.${MACHINE}/f77 f77.${MACHINE}/f77pass1 fpr fsplit

all f77 clean lint tags: FRC
	for i in ${SUBDIR}; \
		do (cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} $@); done

cleandir: clean
	for i in ${SUBDIR}; \
		do (cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} $@); done
	rm -f ${MAN} tags .depend

install: ${MAN}
	for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} $@); done
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat1

FRC:
