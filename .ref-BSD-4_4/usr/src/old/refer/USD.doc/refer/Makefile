#	@(#)Makefile	8.1 (Berkeley) 6/8/93

# refer.mac should be typed on a 12 pitch Daisy wheel printer
DIR=	usd/26.refer
SRCS=	refer.bib
MACROS=	-msU
REFER=	refer -p biblio
CLEANFILES=biblio.ig

paper.ps: ${SRCS}
	${REFER} ${SRCS} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
