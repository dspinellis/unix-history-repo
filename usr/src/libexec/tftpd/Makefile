#
# Copyright (c) 1988 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that this notice is preserved and that due credit is given
# to the University of California at Berkeley. The name of the University
# may not be used to endorse or promote products derived from this
# software without specific prior written permission. This software
# is provided ``as is'' without express or implied warranty.
#
#	@(#)Makefile	5.6 (Berkeley) %G%
#
VPATH=	../../ucb/tftp
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
tftpd.o: /usr/include/sys/ttydefaults.h /usr/include/sys/ttydev.h
tftpd.o: /usr/include/sys/wait.h /usr/include/machine/endian.h
tftpd.o: /usr/include/sys/stat.h /usr/include/netinet/in.h
tftpd.o: /usr/include/arpa/tftp.h /usr/include/signal.h
tftpd.o: /usr/include/machine/trap.h /usr/include/stdio.h /usr/include/errno.h
tftpd.o: /usr/include/ctype.h /usr/include/netdb.h /usr/include/setjmp.h
tftpd.o: /usr/include/syslog.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
