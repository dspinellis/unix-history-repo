#	@(#)Makefile	5.13 (Berkeley) 7/25/90

PROG=	nimd
MAN8=	nimd.0
SRCS=	buf.c commands.c nim.c x29.c
DPADD=	${LIBUTIL}
LDADD=	-lutil -lx25
CFLAGS= -DCCITT

.include <bsd.prog.mk>
