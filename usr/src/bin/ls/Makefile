#	@(#)Makefile	5.4 (Berkeley) %G%

PROG=	ls
SRCS=	cmp.c getbsize.c ls.c print.c util.c
.PATH:	${.CURDIR}/../../bin/df
DPADD=	${LIBUTIL}
LDADD=	-lutil

.include <bsd.prog.mk>
