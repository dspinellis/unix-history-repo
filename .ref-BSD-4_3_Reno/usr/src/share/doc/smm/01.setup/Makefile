#	@(#)Makefile	5.2 (Berkeley) 6/30/90

DIR=	smm/01.setup
SRCS=	0.t 1.t 2.t 3.t 4.t 5.t 6.t a.t b.t
.if ${MACHINE} == "vax"
SRCS+=	c.t
.endif
.PATH:	${.CURDIR}/${MACHINE} ${.CURDIR}/common

paper.${PRINTER}: ${SRCS}
	${EQN} ${.ALLSRC} | ${TBL} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
