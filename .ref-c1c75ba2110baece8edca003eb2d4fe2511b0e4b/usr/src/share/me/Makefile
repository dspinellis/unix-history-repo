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
#	@(#)Makefile	6.7 (Berkeley) %G%
#
MELIB=	/usr/lib/me
TMLIB=	/usr/lib/tmac
MESRCS=	acm.me chars.me deltext.me eqn.me float.me footnote.me \
	index.me letterhead.me local.me null.me refer.me sh.me tbl.me thesis.me
TMSRCS=	tmac.e

all:

install: FRC
	-[ -d ${DESTDIR}${MELIB} ] || mkdir ${DESTDIR}${MELIB}
	-[ -d ${DESTDIR}${TMLIB} ] || mkdir ${DESTDIR}${TMLIB}
	chown bin.bin ${DESTDIR}${MELIB} ${DESTDIR}${TMLIB}
	chmod 755 ${DESTDIR}${MELIB} ${DESTDIR}${TMLIB}
	for i in ${MESRCS}; do \
		sed -f strip.sed < $$i > ${DESTDIR}${MELIB}/$$i; \
		chown bin.bin ${DESTDIR}${MELIB}/$$i; \
		chmod 444 ${DESTDIR}${MELIB}/$$i; \
	done
	for i in ${TMSRCS}; do \
		sed -f strip.sed < $$i > ${DESTDIR}${TMLIB}/$$i; \
		chown bin.bin ${DESTDIR}${TMLIB}/$$i; \
		chmod 444 ${DESTDIR}${TMLIB}/$$i; \
	done
	install -c -o bin -g bin -m 444 revisions ${DESTDIR}${MELIB}/revisions

clean:
cleandir:
depend:
lint:
tags:
FRC:
