#	@(#)Makefile	5.2 (Berkeley) %G%

FILES=	bsd.lib.mk bsd.man.mk bsd.own.mk bsd.prog.mk bsd.subdir.mk sys.mk
NOOBJ=	noobj

all clean cleandir depend lint tags:

install:
	cd ${.CURDIR}; install -c -o ${BINOWN} -g ${BINOWN} -m 444 ${FILES} \
	    ${DESTDIR}${BINDIR}/mk

.include <bsd.prog.mk>
