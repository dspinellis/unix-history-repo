#	@(#)Makefile	5.10 (Berkeley) %G%

PROG=	vfontedpr
SRCS=	regexp.c vfontedpr.c
MAN1=	vgrind.0
MAN5=	vgrindefs.0
BINDIR=	/usr/libexec
CLEANFILES+= vgrindefs.src.db

vgrindefs.src.db: vgrindefs.src

beforeinstall:
	cap_mkdb ${.CURDIR}/vgrindefs.src
	install -c -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} \
	    ${.CURDIR}/vgrind.sh ${DESTDIR}/usr/bin/vgrind
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${.CURDIR}/vgrindefs.src \
	    ${DESTDIR}/usr/share/misc/vgrindefs
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 \
	    ${.CURDIR}/vgrindefs.src.db ${DESTDIR}/usr/share/misc/vgrindefs.db
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${.CURDIR}/tmac.vgrind \
	    ${DESTDIR}/usr/share/tmac

.include <bsd.prog.mk>
