#	@(#)Makefile	8.1 (Berkeley) 6/8/93

DIR=	smm/01.setup/vax
SRCS=	0.t 1.t 2.t ../common/3.t ../common/4.t ../common/5.t \
	../common/6.t a.t b.t c.t
FILES=	${SRCS} ../common/renohints.t
MACROS=	-ms

paper.ps: ${SRCS}
	${EQN} ${SRCS} | ${TBL} | ${ROFF} > ${.TARGET}

install: ${SRCS}
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 \
	    Makefile ${FILES} ${EXTRA} ${DESTDIR}${BINDIR}/${DIR}

.include <bsd.doc.mk>
