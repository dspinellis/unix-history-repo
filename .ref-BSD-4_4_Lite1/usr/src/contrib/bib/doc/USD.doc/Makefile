#	@(#)Makefile	8.1 (Berkeley) 6/8/93


DIR=	usd/28.bib
SRCS=	char.macros bibdoc.ms
MACROS=	-ms
CLEANFILES+=INDEX

paper.ps: ${SRCS} INDEX
	/usr/contrib/bin/bib -Tib ${SRCS} | ${TBL} | ${ROFF} > ${.TARGET}

INDEX:	testrefs
	/usr/contrib/bin/invert testrefs

.include <bsd.doc.mk>
