#	@(#)Makefile	5.1 (Berkeley) %G%

PROG=	style2
CFLAGS+=-I${.CURDIR} -I${.CURDIR}/../style1
SRCS=	style2.c
DPADD=	${LIBL}
LDADD=	-ll
NOMAN=	noman
BINDIR=	/usr/libexec

.include <bsd.prog.mk>
