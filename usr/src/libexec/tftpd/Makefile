#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.4	(Berkeley)	%G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	tftpd.c tftpsubs.c
OBJS=	tftpd.o tftpsubs.o

all: tftpd

tftpd:	${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS}

clean: FRC
	rm -f ${OBJS} core tftpd

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 tftpd ${DESTDIR}/etc/tftpd

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

tftpd.o: tftpd.c /usr/include/sys/types.h /usr/include/sys/socket.h
tftpd.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
tftpd.o: /usr/include/sys/ttydev.h /usr/include/sys/wait.h
tftpd.o: /usr/include/machine/machparam.h /usr/include/sys/stat.h
tftpd.o: /usr/include/netinet/in.h /usr/include/arpa/tftp.h
tftpd.o: /usr/include/signal.h /usr/include/stdio.h /usr/include/errno.h
tftpd.o: /usr/include/ctype.h /usr/include/netdb.h /usr/include/setjmp.h
tftpd.o: /usr/include/syslog.h
tftpsubs.o: tftpsubs.c /usr/include/sys/types.h /usr/include/sys/socket.h
tftpsubs.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
tftpsubs.o: /usr/include/sys/ttydev.h /usr/include/netinet/in.h
tftpsubs.o: /usr/include/arpa/tftp.h /usr/include/stdio.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
