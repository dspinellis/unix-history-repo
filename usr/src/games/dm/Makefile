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
#	@(#)Makefile	5.3 (Berkeley) 6/18/88
#
# -DLOG		log games in /usr/adm
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	dm.c
OBJS=

all: dm

dm: ${LIBC}
	${CC} -o $@ ${CFLAGS} dm.c

clean: FRC
	rm -f core dm

depend: FRC
	mkdep -p ${CFLAGS} ${SRCS}

install: FRC
	install -s -o games -g kmem -m 6755 dm ${DESTDIR}/usr/games
#	install -c -o games -g bin -m 444 dm.config ${DESTDIR}/usr/games

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

dm: dm.c /usr/include/sys/param.h /usr/include/sys/types.h
dm: /usr/include/signal.h /usr/include/machine/trap.h
dm: /usr/include/machine/machparam.h /usr/include/machine/endian.h
dm: /usr/include/sys/file.h /usr/include/sys/time.h /usr/include/time.h
dm: /usr/include/sys/resource.h /usr/include/pwd.h /usr/include/utmp.h
dm: /usr/include/nlist.h /usr/include/stdio.h /usr/include/ctype.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
