
PROG=	crontab
SRCS=	crontab.c misc.c entry.c env.c
CFLAGS+=-I${.CURDIR} -I${.CURDIR}/../../libexec/crond -DDEBUGGING=1 -DBSD -DCRONDIR='"/var/cron"' -fstrength-reduce
MAN5=	crontab.0

.include <bsd.prog.mk>
.PATH: ${.CURDIR}/../../libexec/crond
