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
#	@(#)Makefile	5.5 (Berkeley) %G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	ftpd.c ftpcmd.c glob.c vers.c
OBJS=	ftpd.o ftpcmd.o glob.o vers.o

all: ftpd

ftpd: ${OBJS} ${LIBC}
	${CC} -o $@ ${OBJS}

vers.o: ftpd.c ftpcmd.y
	sh newvers.sh
	${CC} ${CFLAGS} -c vers.c

clean: FRC
	rm -f ${OBJS} ftpd core ftpcmd.c

depend: ${SRCS} FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 ftpd ${DESTDIR}/etc/ftpd

lint: ${SRCS} FRC
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS} FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

ftpd.o: ftpd.c /usr/include/sys/param.h /usr/include/sys/types.h
ftpd.o: /usr/include/signal.h /usr/include/machine/trap.h
ftpd.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
ftpd.o: /usr/include/sys/stat.h /usr/include/sys/ioctl.h
ftpd.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
ftpd.o: /usr/include/sys/socket.h /usr/include/sys/file.h
ftpd.o: /usr/include/sys/wait.h /usr/include/netinet/in.h
ftpd.o: /usr/include/arpa/ftp.h /usr/include/arpa/inet.h
ftpd.o: /usr/include/arpa/telnet.h /usr/include/stdio.h /usr/include/signal.h
ftpd.o: /usr/include/pwd.h /usr/include/setjmp.h /usr/include/netdb.h
ftpd.o: /usr/include/errno.h /usr/include/strings.h /usr/include/syslog.h
ftpd.o: /usr/include/utmp.h
ftpcmd.o: ftpcmd.c /usr/include/sys/types.h /usr/include/sys/socket.h
ftpcmd.o: /usr/include/netinet/in.h /usr/include/arpa/ftp.h
ftpcmd.o: /usr/include/stdio.h /usr/include/signal.h
ftpcmd.o: /usr/include/machine/trap.h /usr/include/ctype.h /usr/include/pwd.h
ftpcmd.o: /usr/include/setjmp.h /usr/include/syslog.h
ftpcmd.o: /usr/include/arpa/telnet.h
glob.o: glob.c /usr/include/sys/param.h /usr/include/sys/types.h
glob.o: /usr/include/signal.h /usr/include/machine/trap.h
glob.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
glob.o: /usr/include/sys/stat.h /usr/include/sys/dir.h /usr/include/stdio.h
glob.o: /usr/include/errno.h /usr/include/pwd.h
vers.o: vers.c

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
