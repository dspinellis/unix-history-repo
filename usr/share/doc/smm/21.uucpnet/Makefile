#	@(#)Makefile	6.2 (Berkeley) 6/30/90

DIR=	smm/21.uucpnet
SRCS=	network.ms
MACROS=	-msU
REFER=	refer -p /usr/doc/run/Ind

paper.${PRINTER}: ${SRCS}
	${REFER} -e ${SRCS} | ${TBL} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
