#	@(#)Makefile	8.1 (Berkeley) 6/8/93

DIR=	smm/08.sendmailop
SRCS=	op.me
MACROS=	-me

all: op.ps

op.ps: ${SRCS}
	${PIC} ${SRCS} | ${EQN} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
