#	@(#)Makefile	5.11 (Berkeley) 7/2/90

PROG=	getty
SRCS=	main.c init.c subr.c gettytab.c get_date.c ttydefaults.c
DPADD=	${LIBUTIL}
LDADD=	-lutil
MAN5=	gettytab.0 ttys.0
MAN8=	getty.0

.include <bsd.prog.mk>
