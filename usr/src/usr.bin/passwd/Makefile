#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.2	(Berkeley)	%G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	passwd.c

all: passwd

passwd: ${LIBC}
	${CC} -o $@ ${CFLAGS} ${SRCS}

clean: FRC
	rm -f core passwd

depend: FRC
	mkdep -p ${CFLAGS} ${SRCS}

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

passwd: passwd.c /usr/include/sys/types.h /usr/include/sys/file.h
passwd: /usr/include/sys/stat.h /usr/include/sys/time.h /usr/include/time.h
passwd: /usr/include/sys/resource.h /usr/include/stdio.h /usr/include/signal.h
passwd: /usr/include/machine/trap.h /usr/include/pwd.h /usr/include/ndbm.h
passwd: /usr/include/errno.h /usr/include/strings.h /usr/include/ctype.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
