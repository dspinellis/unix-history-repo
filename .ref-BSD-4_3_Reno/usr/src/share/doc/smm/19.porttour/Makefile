#	@(#)Makefile	6.3 (Berkeley) 6/30/90

DIR=	smm/19.porttour
SRCS=	porttour1.ms porttour2.ms
MACROS=	-msU
REFER=	refer -p /usr/doc/run/Ind

paper.${PRINTER}: ${SRCS}
	${REFER} -e ${SRCS} | ${TBL} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
