#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.3	(Berkeley)	%G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	msgs.c
OBJS=	msgs.o

all: msgs

msgs:	${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS} -ltermlib

clean: FRC
	rm -f ${OBJS} core msgs

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 msgs ${DESTDIR}/usr/ucb/msgs

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

msgs.o: msgs.c /usr/include/stdio.h /usr/include/sys/param.h
msgs.o: /usr/include/sys/types.h /usr/include/signal.h
msgs.o: /usr/include/machine/machparam.h /usr/include/signal.h
msgs.o: /usr/include/sys/dir.h /usr/include/sys/stat.h /usr/include/ctype.h
msgs.o: /usr/include/pwd.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
msgs.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
msgs.o: /usr/include/setjmp.h
msgs.o:msgs.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
