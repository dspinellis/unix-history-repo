#	@(#)Makefile	1.2 (Berkeley) %G%

FILES=	tmac.an.v6compat tmac.an6n tmac.an6t tmac.an

NOOBJ=	noobj
BINDIR= /usr/old/lib

all clean cleandir depend lint tags:

beforeinstall:
	install -c -o ${BINOWN} -g ${BINOWN} -m 444 ${FILES} \
		${DESTDIR}${BINDIR}/tmac

BINDIR= /usr/old/lib

.include <bsd.prog.mk>
