#
# Copyright (c) 1987 Regents of the University of California.
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
SRCS=	sysline.c
OBJS=

all: sysline

sysline: ${LIBC}
	${CC} ${CFLAGS} -o $@ sysline.c -ltermlib -lcurses

clean: FRC
	rm -f core sysline

depend: FRC
	mkdep -p ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g kmem -m 2755 sysline ${DESTDIR}/usr/ucb/sysline

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

sysline: sysline.c /usr/include/stdio.h /usr/include/sys/param.h
sysline: /usr/include/sys/types.h /usr/include/signal.h
sysline: /usr/include/machine/trap.h /usr/include/machine/machparam.h
sysline: /usr/include/machine/endian.h /usr/include/sys/signal.h
sysline: /usr/include/utmp.h /usr/include/ctype.h /usr/include/sys/time.h
sysline: /usr/include/time.h /usr/include/sys/stat.h /usr/include/nlist.h
sysline: /usr/include/sys/vtimes.h /usr/include/sys/proc.h
sysline: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
sysline: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
sysline: /usr/include/sys/ttydefaults.h /usr/include/sys/ttydev.h
sysline: /usr/include/protocols/rwhod.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
