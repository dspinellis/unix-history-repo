#	$Id: Makefile,v 1.1 1993/09/16 00:27:42 jtc Exp $

PROG =	rpc.rstatd
SRCS =	rstatd.c rstat_proc.c
MAN8 =	rpc.rstatd.8

DPADD=	${LIBRPCSVC} ${LIBUTIL}
LDADD=	-lrpcsvc -lutil

.include <bsd.prog.mk>
