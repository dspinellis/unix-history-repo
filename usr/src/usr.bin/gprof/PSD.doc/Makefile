#	@(#)Makefile	5.4 (Berkeley) %G%

DIR=	psd/18.gprof
SRCS=	header.me abstract.me intro.me profiling.me gathering.me \
	postp.me present.me refs.me
DPADD=	postp1.pic postp2.pic postp3.pic pres1.pic pres2.pic
MACROS=	-me

paper.ps: ${SRCS} ${DPADD}
	${SOELIM} ${SRCS} | ${PIC} | ${TBL} | ${EQN} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
