#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.4	(Berkeley)	%G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	main.c tftp.c tftpsubs.c
OBJS=	main.o tftp.o tftpsubs.o

all: tftp

tftp:	${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS}

clean: FRC
	rm -f ${OBJS} core tftp

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 tftp ${DESTDIR}/usr/ucb/tftp

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

main.o: main.c /usr/include/sys/types.h /usr/include/sys/socket.h
main.o: /usr/include/sys/file.h /usr/include/netinet/in.h /usr/include/signal.h
main.o: /usr/include/stdio.h /usr/include/errno.h /usr/include/setjmp.h
main.o: /usr/include/ctype.h /usr/include/netdb.h
tftp.o: tftp.c /usr/include/sys/types.h /usr/include/sys/socket.h
tftp.o: /usr/include/sys/time.h /usr/include/time.h /usr/include/netinet/in.h
tftp.o: /usr/include/arpa/tftp.h /usr/include/signal.h /usr/include/stdio.h
tftp.o: /usr/include/errno.h /usr/include/setjmp.h
tftpsubs.o: tftpsubs.c /usr/include/sys/types.h /usr/include/sys/socket.h
tftpsubs.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
tftpsubs.o: /usr/include/sys/ttydev.h /usr/include/netinet/in.h
tftpsubs.o: /usr/include/arpa/tftp.h /usr/include/stdio.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
