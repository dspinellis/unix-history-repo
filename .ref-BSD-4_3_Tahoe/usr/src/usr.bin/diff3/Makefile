#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.3	(Berkeley)	6/18/87
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	diff3.c
OBJS=	diff3.o

all: diff3

diff3: diff3.c ${LIBC}
	${CC} ${CFLAGS} -o $@ diff3.c

clean: FRC
	rm -f ${OBJS} core diff3

depend: FRC
	mkdep -p ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 diff3 $(DESTDIR)/usr/lib/diff3
	install -c -o bin -g bin -m 755 diff3.sh $(DESTDIR)/usr/bin/diff3

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

diff3: diff3.c /usr/include/stdio.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
