#	@(#)Makefile	1.5 (Berkeley) %G%

DIR=	papers/relengr
SRCS=	0.t 1.t 2.t 3.t
MACROS=	-ms
EXTRA=	ref.bib tmac.srefs
REFER=	/a/staff/mckusick/book/ref/refer -m -n -e -l -s -p ref.bib

paper.${PRINTER}: ${SRCS}
	${REFER} ${SRCS} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
