#	@(#)Makefile	5.6 (Berkeley) 9/27/90

PROG=	rshd
SRCS=	rshd.c
MAN8=	rshd.8
DPADD=	${LIBUTIL}
LDADD=	-lutil
.PATH: ${.CURDIR}/../../usr.bin/rlogin

.include <bsd.prog.mk>
