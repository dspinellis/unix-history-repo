#	@(#)Makefile	6.4 (Berkeley) %G%

DIR=	ps1/07.ipctut
SRCS=	tutor.me
GSRCS=	gremlins
MACROS=	-me
EXTRA=	accept.grn dgramread.c dgramsend.c gremlins pipe.c pipe.grn \
	socketpair.c socketpair.grn strchkread.c streamread.c streamwrite.c \
	udgramread.c udgramsend.c ustreamread.c ustreamwrite.c

print: paper.${PRINTER} pics.${PRINTER}
	lpr -P${PRINTER} -n paper.${PRINTER}
	lpr -P${PRINTER} -n pics.P${PRINTER

paper.${PRINTER}: ${SRCS}
	${TBL} ${SRCS} | ${ROFF} > ${.TARGET}

pics.${PRINTER}: ${GSRCS}
	${GREMLIN} ${GSRCS} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
