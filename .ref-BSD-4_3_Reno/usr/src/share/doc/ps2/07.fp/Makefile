#	@(#)Makefile	6.2 (Berkeley) 6/30/90

DIR=	ps2/07.fp
SRCS = 	manDefs.rno manCh0.rno manCh1.rno manCh2.rno manCh3.rno manCh4.rno \
	manCh5.rno manCh6.rno manCh7.rno refs.rno manApp.rno
MACROS= -me

paper.${PRINTER}: ${SRCS}
	${TBL} ${SRCS} | ${EQN} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
