#	@(#)Makefile	5.7 (Berkeley) %G%

PROG=	syslogd
SRCS=	syslogd.c ttymsg.c
.PATH:	${.CURDIR}/../../usr.bin/wall
MAN5=	syslog.conf.0
MAN8=	syslogd.0
LDADD=	-lutil

.include <bsd.prog.mk>
