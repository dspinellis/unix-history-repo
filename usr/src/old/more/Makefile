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
#	@(#)Makefile	5.4 (Berkeley) %G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	more.c
OBJS=	more.o

all: more

more:	${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS} -ltermcap

clean: FRC
	rm -f ${OBJS} core more

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 more ${DESTDIR}/usr/ucb/more
	rm -f ${DESTDIR}/usr/ucb/page
	ln ${DESTDIR}/usr/ucb/more ${DESTDIR}/usr/ucb/page
	install -c -o bin -g bin -m 644 more.help ${DESTDIR}/usr/lib/more.help

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

more.o: more.c /usr/include/stdio.h /usr/include/sys/types.h
more.o: /usr/include/ctype.h /usr/include/signal.h /usr/include/errno.h
more.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
more.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
more.o: /usr/include/setjmp.h /usr/include/sys/stat.h /usr/include/sys/file.h
more.o: /usr/include/sys/exec.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
