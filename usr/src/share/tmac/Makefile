#	@(#)Makefile	6.5 (Berkeley) %G%

FILES=	tmac.an tmac.an.new tmac.an6n tmac.an6t

all clean cleandir depend lint tags:

install:
	install -c -o ${BINOWN} -g ${BINOWN} -m 444 ${FILES} \
	    ${DESTDIR}${BINDIR}/tmac

.include <bsd.own.mk>
