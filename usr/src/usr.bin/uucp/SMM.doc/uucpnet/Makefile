#	@(#)Makefile	6.2 (Berkeley) %G%

DIR=	smm/21.uucpnet
SRCS=	network.ms
MACROS=	-ms
REFER=	refer -p /usr/doc/run/Ind

paper.${PRINTER}: ${SRCS}
	${REFER} -e ${SRCS} | ${TBL} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
