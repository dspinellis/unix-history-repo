#	@(#)Makefile	1.2 (Berkeley) %G%

PROG=	xroach
MAN6=	xroach
CFLAGS+= -I${.CURDIR}/bitmaps
DPADD=	${LIBX} ${LIBM}
LDADD=	-lX11/X11 -lm

.include <bsd.prog.mk>
