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
#	@(#)Makefile	5.2 (Berkeley) %G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	cpio.c
OBJS=
MAN=	cpio.0 cpio_format.0

.SUFFIXES: .5 .1 .0

.1.0:
	${DESTDIR}/usr/man/manroff $*.1 > $*.0
.5.0:
	${DESTDIR}/usr/man/manroff $*.5 > $*.0

all: cpio

cpio: ${LIBC}
	${CC} -o $@ ${CFLAGS} cpio.c

clean: FRC
	rm -f ${OBJS} ${MAN} core cpio

depend: FRC
	mkdep -p ${CFLAGS} ${SRCS}

install: ${MAN} FRC
	install -s -o bin -g bin -m 755 cpio ${DESTDIR}/bin/cpio
	install -c -o bin -g bin -m 444 cpio.0 ${DESTDIR}/usr/man/cat1/cpio.0
	install -c -o bin -g bin -m 444 cpio_format.0 ${DESTDIR}/usr/man/cat5/cpio.0

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

cpio: cpio.c /usr/include/errno.h /usr/include/fcntl.h /usr/include/memory.h
cpio: /usr/include/stdio.h /usr/include/string.h /usr/include/strings.h
cpio: /usr/include/signal.h /usr/include/machine/trap.h /usr/include/varargs.h
cpio: /usr/include/sys/param.h /usr/include/sys/types.h /usr/include/signal.h
cpio: /usr/include/machine/machparam.h /usr/include/machine/endian.h
cpio: /usr/include/sys/types.h /usr/include/sys/stat.h /usr/include/pwd.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
