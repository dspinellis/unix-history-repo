#	@(#)Makefile	6.6 (Berkeley) %G%

DIR=	psd/20.ipctut
SRCS=	tutor.me
MACROS=	-me
EXTRA=	accept.grn dgramread.c dgramsend.c gremlins pipe.c pipe.grn \
	socketpair.c socketpair.grn strchkread.c streamread.c streamwrite.c \
	udgramread.c udgramsend.c ustreamread.c ustreamwrite.c

paper.ps: ${SRCS} ${EXTRA}
	${TBL} ${SRCS} | ${GREMLIN} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
