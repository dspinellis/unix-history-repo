#	$Id: Makefile,v 1.4 1993/08/02 17:50:53 mycroft Exp $

PROG =	rpc.rwalld
SRCS =	rwalld.c
MAN8 =	rpc.rwalld.8

DPADD=	${LIBRPCSVC} ${LIBRPC} ${LIBUTIL}
LDADD=	-lrpcsvc -lrpc -lutil

.include <bsd.prog.mk>
