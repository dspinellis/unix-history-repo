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
#	@(#)Makefile	5.3 (Berkeley) 7/9/88
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	arff.c bad144.c flcopy.c rxformat.c
ALL=	arff bad144 flcopy rxformat

all: ${ALL}

${ALL}: ${LIBC}
	${CC} ${CFLAGS} -o $@ $@.c

clean: FRC
	rm -f ${ALL} core

depend: FRC
	mkdep -p ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 arff ${DESTDIR}/etc/arff
	install -s -o bin -g bin -m 755 bad144 ${DESTDIR}/etc/bad144
	install -s -o bin -g bin -m 755 flcopy ${DESTDIR}/etc/flcopy
	install -s -o bin -g bin -m 755 rxformat ${DESTDIR}/etc/rxformat

lint:
tags:
FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

arff: arff.c /usr/include/sys/types.h /usr/include/sys/stat.h
arff: /usr/include/sys/time.h /usr/include/time.h /usr/include/signal.h
arff: /usr/include/machine/trap.h /usr/include/stdio.h /usr/include/sys/file.h
bad144: bad144.c /usr/include/sys/param.h /usr/include/sys/types.h
bad144: /usr/include/signal.h /usr/include/machine/trap.h
bad144: /usr/include/machine/machparam.h /usr/include/sys/dkbad.h
bad144: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
bad144: /usr/include/sys/ttydev.h /usr/include/sys/fs.h /usr/include/sys/file.h
bad144: /usr/include/stdio.h /usr/include/sys/disklabel.h
flcopy: flcopy.c /usr/include/sys/file.h
rxformat: rxformat.c /usr/include/stdio.h /usr/include/sys/file.h
rxformat: /usr/include/errno.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
