#	@(#)Makefile	1.7 (Berkeley) %G%

DIR=	papers/memfs
SRCS=	0.t 1.t
MACROS=	-ms
REFER=	refer -n -e -l -s -p ref.bib
EXTRA=	ref.bib A.t tmac.srefs
CLEANFILES=ref.bib.i A.gt paper.t
PRINTER=ps

paper.${PRINTER}: paper.t
	${ROFF} tmac.srefs paper.t > ${.TARGET}

paper.t: ${SRCS} ref.bib.i A.gt
	${REFER} ${SRCS} A.gt > ${.TARGET}

ref.bib.i: ref.bib
	${INDXBIB} ref.bib

A.gt: A.t
	${GRIND} < A.t > A.gt

.include <bsd.doc.mk>
