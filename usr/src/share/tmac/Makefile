#	@(#)Makefile	6.7 (Berkeley) %G%

FILES=	tmac.an tmac.r tmac.doc tmac.andoc
NOOBJ=	noobj

# old macros not currently installed...
OFILES=	tmac.an.v6compat tmac.an6n tmac.an6t tmac.a tmac.cp

all clean cleandir depend lint tags:

install:
	install -c -o ${BINOWN} -g ${BINOWN} -m 444 ${FILES} \
	    ${DESTDIR}${BINDIR}/tmac

.include <bsd.prog.mk>
