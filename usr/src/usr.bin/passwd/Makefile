#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.1	(Berkeley)	%G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	passwd.c
OBJS=	passwd.o

all: passwd

passwd:	${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS}

clean: FRC
	rm -f ${OBJS} core passwd

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o root -g bin -m 4755 passwd ${DESTDIR}/bin/passwd
	rm -f ${DESTDIR}/bin/chfn; ln ${DESTDIR}/bin/passwd ${DESTDIR}/bin/chfn
	rm -f ${DESTDIR}/bin/chsh; ln ${DESTDIR}/bin/passwd ${DESTDIR}/bin/chsh

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

passwd.o: passwd.c /usr/include/sys/types.h /usr/include/sys/file.h
passwd.o: /usr/include/sys/stat.h /usr/include/sys/time.h /usr/include/time.h
passwd.o: /usr/include/sys/resource.h /usr/include/stdio.h
passwd.o: /usr/include/signal.h /usr/include/machine/trap.h /usr/include/pwd.h
passwd.o: /usr/include/ndbm.h /usr/include/errno.h /usr/include/strings.h
passwd.o: /usr/include/ctype.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
