#	@(#)Makefile	6.9 (Berkeley) %G%

MELIB=	${BINDIR}/me
TMLIB=	${BINDIR}/tmac
MESRCS=	acm.me chars.me deltext.me eqn.me float.me footnote.me \
	index.me letterhead.me local.me null.me refer.me sh.me \
	tbl.me thesis.me
TMSRCS=	tmac.e
MAN7=	me.0

all: ${MAN7}

clean cleandir depend lint tags:

install: maninstall
	for i in ${MESRCS}; do \
		sed -f strip.sed < $$i > ${DESTDIR}${MELIB}/$$i; \
		chown ${BINOWN}.${BINGRP} ${DESTDIR}${MELIB}/$$i; \
		chmod 444 ${DESTDIR}${MELIB}/$$i; \
	done
	for i in ${TMSRCS}; do \
		sed -f strip.sed < $$i > ${DESTDIR}${TMLIB}/$$i; \
		chown ${BINOWN}.${BINGRP} ${DESTDIR}${TMLIB}/$$i; \
		chmod 444 ${DESTDIR}${TMLIB}/$$i; \
	done
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 revisions \
	    ${DESTDIR}${MELIB}/revisions

.include <bsd.own.mk>
.include <bsd.man.mk>
