#	@(#)Makefile	5.6 (Berkeley) 6/29/90

PROG=	syslogd
SRCS=	syslogd.c ttymsg.c
.PATH:	${.CURDIR}/../../usr.bin/wall
MAN8=	syslogd.0
LDADD=	-lutil

.include <bsd.prog.mk>
