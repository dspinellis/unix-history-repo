#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that this notice is preserved and that due credit is given
# to the University of California at Berkeley. The name of the University
# may not be used to endorse or promote products derived from this
# software without specific written prior permission. This software
# is provided ``as is'' without express or implied warranty.
#
#	@(#)Makefile	5.1 (Berkeley) %G%
#
# -DLOG		log games in /usr/adm
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	dm.c
OBJS=	dm.o

all: dm

dm: ${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS}

clean: FRC
	rm -f ${OBJS} core dm

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

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

dm.o: dm.c /usr/include/sys/param.h /usr/include/sys/types.h
dm.o: /usr/include/signal.h /usr/include/machine/trap.h
dm.o: /usr/include/machine/machparam.h /usr/include/sys/file.h
dm.o: /usr/include/sys/time.h /usr/include/time.h /usr/include/sys/resource.h
dm.o: /usr/include/pwd.h /usr/include/utmp.h /usr/include/nlist.h
dm.o: /usr/include/stdio.h /usr/include/ctype.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
