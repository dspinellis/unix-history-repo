#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.4	(Berkeley)	%G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	htable.c parse.c scan.c
OBJS=	htable.o parse.o scan.o

all: htable

htable: ${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS}

y.tab.h:
	yacc -d parse.y
	rm -f y.tab.c

clean: FRC
	rm -f ${OBJS} core htable parse.c scan.c y.tab.h

depend: ${SRCS} y.tab.h FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 htable ${DESTDIR}/etc/htable

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

htable.o: htable.c /usr/include/stdio.h /usr/include/ctype.h
htable.o: /usr/include/errno.h /usr/include/netdb.h htable.h
htable.o: /usr/include/sys/types.h /usr/include/sys/socket.h
htable.o: /usr/include/netinet/in.h
parse.o: parse.c htable.h /usr/include/sys/types.h /usr/include/stdio.h
scan.o: scan.c /usr/include/stdio.h y.tab.h htable.h /usr/include/sys/types.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
