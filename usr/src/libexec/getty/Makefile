#	@(#)Makefile	5.12 (Berkeley) 1/21/91

PROG=	getty
SRCS=	main.c init.c subr.c gettytab.c ttydefaults.c
DPADD=	${LIBUTIL}
LDADD=	-lutil
MAN5=	gettytab.0 ttys.0
MAN8=	getty.0

.include <bsd.prog.mk>
