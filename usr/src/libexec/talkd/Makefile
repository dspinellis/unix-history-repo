#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.5	(Berkeley)	%G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	talkd.c announce.c process.c table.c print.c
OBJS=	talkd.o announce.o process.o table.o print.o

all: talkd

talkd:	${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS}

clean: FRC
	rm -f ${OBJS} core talkd

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 talkd ${DESTDIR}/etc/ntalkd

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

talkd.o: talkd.c /usr/include/stdio.h /usr/include/errno.h
talkd.o: /usr/include/signal.h /usr/include/syslog.h
talkd.o: /usr/include/protocols/talkd.h /usr/include/sys/types.h
talkd.o: /usr/include/sys/socket.h
announce.o: announce.c /usr/include/sys/types.h /usr/include/sys/stat.h
announce.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
announce.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
announce.o: /usr/include/sys/ioctl.h /usr/include/sys/time.h
announce.o: /usr/include/time.h /usr/include/stdio.h /usr/include/sys/wait.h
announce.o: /usr/include/machine/machparam.h /usr/include/errno.h
announce.o: /usr/include/syslog.h /usr/include/protocols/talkd.h
announce.o: /usr/include/sys/types.h /usr/include/sys/socket.h
process.o: process.c /usr/include/sys/types.h /usr/include/sys/stat.h
process.o: /usr/include/stdio.h /usr/include/syslog.h /usr/include/netdb.h
process.o: /usr/include/netinet/in.h /usr/include/protocols/talkd.h
process.o: /usr/include/sys/types.h /usr/include/sys/socket.h
process.o: /usr/include/utmp.h
table.o: table.c /usr/include/stdio.h /usr/include/sys/time.h
table.o: /usr/include/time.h /usr/include/syslog.h
table.o: /usr/include/protocols/talkd.h /usr/include/sys/types.h
table.o: /usr/include/sys/socket.h
print.o: print.c /usr/include/stdio.h /usr/include/syslog.h
print.o: /usr/include/protocols/talkd.h /usr/include/sys/types.h
print.o: /usr/include/sys/socket.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
