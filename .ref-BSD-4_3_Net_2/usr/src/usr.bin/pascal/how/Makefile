#	@(#)Makefile	5.1 (Berkeley) 6/29/90

FILES=	how_pc how_pi how_pix how_pxp
NOOBJ=	noobj

install:
	install -c -o bin -g bin -m 444 ${FILES} \
	    ${DESTDIR}/usr/libdata/pascal

.include <bsd.prog.mk>
