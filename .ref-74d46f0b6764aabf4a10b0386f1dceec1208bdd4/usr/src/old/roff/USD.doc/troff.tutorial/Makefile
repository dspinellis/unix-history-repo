#	@(#)Makefile	5.1 (Berkeley) %G%

DIR=	usd/25.trofftut
SRCS=	tt.mac tt00 tt01 tt02 tt03 tt04 tt05 tt06 tt07 tt08 tt09 tt10 \
	tt11 tt12 tt13 tt14 ttack ttcharset ttindex
MACROS=	-msU

paper.${PRINTER}: ${SRCS}
	${ROFF} ${SRCS} > ${.TARGET}

.include <bsd.doc.mk>
