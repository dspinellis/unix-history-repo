#	@(#)Makefile	5.1 (Berkeley) %G%

FILES=	how_pc how_pi how_pix how_pxp
NOOBJ=	noobj

install:
	install -c -o bin -g bin -m 444 ${FILES} \
	    ${DESTDIR}/usr/libdata/pascal

.include <bsd.prog.mk>
