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
#	@(#)Makefile	5.6 (Berkeley) %G%
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

lint: ${SRCS} FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

htable.o: htable.c /usr/include/stdio.h /usr/include/ctype.h
htable.o: /usr/include/errno.h /usr/include/netdb.h htable.h
htable.o: /usr/include/sys/types.h /usr/include/netinet/in.h
htable.o: /usr/include/sys/socket.h /usr/include/arpa/inet.h
parse.o: parse.c htable.h /usr/include/sys/types.h /usr/include/netinet/in.h
parse.o: /usr/include/stdio.h
scan.o: scan.c /usr/include/stdio.h y.tab.h htable.h /usr/include/sys/types.h
scan.o: /usr/include/netinet/in.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
