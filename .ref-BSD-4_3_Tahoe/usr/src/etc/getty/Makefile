#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.3	(Berkeley)	6/2/87
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	main.c init.c subr.c gettytab.c get_date.c
OBJS=	main.o init.o subr.o gettytab.o get_date.o

all: getty

getty:	${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS}

clean: FRC
	rm -f ${OBJS} core getty

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 getty ${DESTDIR}/etc/getty
	install -c -o bin -g bin -m 444 gettytab ${DESTDIR}/etc/gettytab

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

main.o: main.c /usr/include/sgtty.h /usr/include/sys/ioctl.h
main.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
main.o: /usr/include/signal.h /usr/include/machine/trap.h /usr/include/ctype.h
main.o: /usr/include/setjmp.h /usr/include/syslog.h /usr/include/sys/file.h
main.o: gettytab.h
init.o: init.c /usr/include/sgtty.h /usr/include/sys/ioctl.h
init.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h gettytab.h
subr.o: subr.c /usr/include/sgtty.h /usr/include/sys/ioctl.h
subr.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h gettytab.h
subr.o: /usr/include/sys/time.h /usr/include/time.h
gettytab.o: gettytab.c /usr/include/ctype.h
get_date.o: get_date.c /usr/include/stdio.h /usr/include/sys/time.h
get_date.o: /usr/include/time.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
