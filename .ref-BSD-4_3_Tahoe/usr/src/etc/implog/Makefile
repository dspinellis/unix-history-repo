#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation,
# advertising materials, and other materials related to such
# distribution and use acknowledge that the software was developed
# by the University of California, Berkeley.  The name of the
# University may not be used to endorse or promote products derived
# from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	5.6 (Berkeley) 6/18/88
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
implog.o: /usr/include/sys/types.h /usr/include/sys/file.h
implog.o: /usr/include/sys/stat.h /usr/include/sys/socket.h
implog.o: /usr/include/net/if.h /usr/include/net/if_arp.h
implog.o: /usr/include/netinet/in.h /usr/include/netimp/if_imp.h
implogd.o: implogd.c /usr/include/sgtty.h /usr/include/sys/ioctl.h
implogd.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
implogd.o: /usr/include/sys/time.h /usr/include/time.h /usr/include/sys/param.h
implogd.o: /usr/include/sys/types.h /usr/include/signal.h
implogd.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
implogd.o: /usr/include/machine/endian.h /usr/include/sys/socket.h
implogd.o: /usr/include/sys/syslog.h /usr/include/sys/file.h
implogd.o: /usr/include/net/if.h /usr/include/net/if_arp.h
implogd.o: /usr/include/netinet/in.h /usr/include/netimp/if_imp.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
