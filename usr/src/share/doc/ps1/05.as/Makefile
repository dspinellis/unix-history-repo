#	@(#)Makefile	1.3	(Berkeley)	6/30/90

DIR=	ps1/05.as
SRCS=	asdocs0.me asdocs1.me asdocs2.me asdocs3.me asdocs4.me
EQNCH=	/usr/share/misc/eqnchar
MACROS=	-me
EXTRA=	as.man.1

paper.${PRINTER}: ${SRCS}
	(cat ${EQNCH}; ${SOELIM} ${SRCS} | ${TBL}) | ${EQN} > ${.TARGET}

.include <bsd.doc.mk>
