#	$Id: Makefile,v 1.4 1993/08/02 17:50:37 mycroft Exp $

PROG =	rpc.rstatd
SRCS =	rstatd.c rstat_proc.c
MAN8 =	rpc.rstatd.8

DPADD=	${LIBRPCSVC} ${LIBRPC} ${LIBUTIL}
LDADD=	-lrpcsvc -lrpc -lutil

.include <bsd.prog.mk>
