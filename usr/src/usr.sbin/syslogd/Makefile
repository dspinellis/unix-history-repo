#	@(#)Makefile	5.6 (Berkeley) %G%

PROG=	syslogd
SRCS=	syslogd.c ttymsg.c
.PATH:	${.CURDIR}/../../usr.bin/wall
MAN8=	syslogd.0
LDADD=	-lutil

.include <bsd.prog.mk>
