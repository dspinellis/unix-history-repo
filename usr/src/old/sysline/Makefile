#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.3	(Berkeley)	%G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	sysline.c
OBJS=	sysline.o

all: sysline

sysline: ${OBJS} ${LIBC}
	${CC} ${CFLAGS} -o $@ ${OBJS} -ltermlib -lcurses

clean: FRC
	rm -f ${OBJS} core sysline

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g kmem -m 2755 sysline ${DESTDIR}/usr/ucb/sysline

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

sysline.o: sysline.c /usr/include/stdio.h /usr/include/sys/param.h
sysline.o: /usr/include/sys/types.h /usr/include/signal.h
sysline.o: /usr/include/machine/machparam.h /usr/include/sys/signal.h
sysline.o: /usr/include/utmp.h /usr/include/ctype.h /usr/include/sys/time.h
sysline.o: /usr/include/time.h /usr/include/sys/stat.h /usr/include/nlist.h
sysline.o: /usr/include/sys/vtimes.h /usr/include/sys/proc.h
sysline.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
sysline.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
sysline.o: /usr/include/sys/ttydev.h /usr/include/protocols/rwhod.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
