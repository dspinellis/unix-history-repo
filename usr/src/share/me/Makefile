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
#	@(#)Makefile	6.4 (Berkeley) %G%
#
MELIB=	/usr/lib/me
TMLIB=	/usr/lib/tmac
MESRCS=	acm.me chars.me deltext.me eqn.me float.me footnote.me \
	index.me local.me null.me refer.me sh.me tbl.me thesis.me
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
depend:
FRC:
