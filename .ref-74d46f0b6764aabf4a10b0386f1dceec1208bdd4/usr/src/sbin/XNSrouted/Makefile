#	@(#)Makefile	5.14 (Berkeley) %G%

PROG=	XNSrouted
MAN8=	XNSrouted.0
SRCS=	af.c if.c input.c main.c output.c startup.c tables.c timer.c trace.c
DPADD=	${LIBUTIL} ${LIBCOMPAT}
LDADD=	-lutil -lcompat

.include <bsd.prog.mk>
