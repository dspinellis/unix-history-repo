#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.3	(Berkeley)	%G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	calendar.c
OBJS=	calendar.o

all: calendar

calendar: calendar.c ${LIBC}
	${CC} ${CFLAGS} -o $@ calendar.c

clean: FRC
	rm -f ${OBJS} core calendar

depend: FRC
	mkdep -p ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 calendar ${DESTDIR}/usr/lib
	install -c -o bin -g bin -m 755 calendar.sh ${DESTDIR}/usr/bin/calendar

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

calendar.o: calendar.c /usr/include/sys/time.h /usr/include/time.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
