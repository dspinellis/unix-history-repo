#	@(#)Makefile	5.2 (Berkeley) 6/30/90

DIR=	smm/07.sendmailop
SRCS=	op.me
MACROS=	-me

paper.${PRINTER}: ${SRCS}
	${PIC} ${SRCS} | ${EQN} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
