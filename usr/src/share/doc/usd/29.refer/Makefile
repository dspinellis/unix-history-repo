#	@(#)Makefile	6.2 (Berkeley) 6/30/90

# refer.mac should be typed on a 12 pitch Daisy wheel printer
DIR=	usd/29.refer
SRCS=	refer.bib
MACROS=	-msU
REFER=	refer -p biblio

paper.${PRINTER}: ${SRCS}
	${REFER} ${SRCS} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
