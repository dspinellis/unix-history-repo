
PROG=	crond
SRCS=	crond.o database.o user.o entry.o misc.o job.o do_command.o env.o
CFLAGS+=-I${.CURDIR} -DDEBUGGING=1 -DBSD -DCRONDIR='"/var/cron"' -fstrength-reduce
MAN8=	crond.0

.include <bsd.prog.mk>
