#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.5	(Berkeley)	%G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	implog.c implogd.c
OBJS1=	implog.o
OBJS2=	implogd.o

all: implog implogd

implog: ${OBJS1} ${LIBC}
	${CC} ${OBJS1} -o $@

implogd: ${OBJS2} ${LIBC}
	${CC} ${OBJS2} -o $@

clean: FRC
	rm -f ${OBJS1} ${OBJS2} core implog implogd

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 implog ${DESTDIR}/etc/implog
	install -s -o bin -g bin -m 755 implogd ${DESTDIR}/etc/implogd

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

implog.o: implog.c /usr/include/stdio.h /usr/include/signal.h
implog.o: /usr/include/machine/trap.h /usr/include/sgtty.h
implog.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
implog.o: /usr/include/sys/ttydev.h /usr/include/sys/time.h /usr/include/time.h
implog.o: /usr/include/sys/types.h /usr/include/sys/stat.h
implog.o: /usr/include/sys/socket.h /usr/include/netinet/in.h
implog.o: /usr/include/netimp/if_imp.h
implogd.o: implogd.c /usr/include/sgtty.h /usr/include/sys/ioctl.h
implogd.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
implogd.o: /usr/include/sys/time.h /usr/include/time.h /usr/include/sys/param.h
implogd.o: /usr/include/sys/types.h /usr/include/signal.h
implogd.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
implogd.o: /usr/include/sys/socket.h /usr/include/sys/syslog.h
implogd.o: /usr/include/sys/file.h /usr/include/netinet/in.h
implogd.o: /usr/include/netimp/if_imp.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
