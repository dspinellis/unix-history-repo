#	@(#)Makefile	8.2 (Berkeley) %G%

# refer.mac should be typed on a 12 pitch Daisy wheel printer
DIR=	usd/26.refer
SRCS=	refer.bib refer.mac
MACROS=	-ms
REFER=	refer -p biblio
CLEANFILES=biblio.ig

paper.ps: ${SRCS}
	${REFER} ${SRCS} | ${TBL} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
