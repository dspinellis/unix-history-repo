#	@(#)Makefile	8.1 (Berkeley) 6/8/93

PROG=	xroach
MAN6=	xroach.0
CFLAGS+= -I${.CURDIR}/bitmaps
DPADD=	${LIBX} ${LIBM}
LDADD=	-lX11/X11 -lm

.include <bsd.prog.mk>
