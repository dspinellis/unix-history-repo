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
#	@(#)Makefile	5.1 (Berkeley) %G%
#
CFLAGS=	-DHERE=TAHOE -DTARGET=TAHOE -DFAMILY=PCC -DUCBVAXASM \
	-DUCBPASS2 -O -I$(INCLDIR)/usr/include -I../include
LIBC=	/lib/libc.a
SRCS=	f77.c
OBJS=	f77.o

all: f77

f77: ${SRCS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${SRCS}

clean: FRC
	rm -f ${OBJS} core f77

depend: FRC
	mkdep -p ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 f77 ${DESTDIR}/usr/bin/f77

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

f77: f77.c /usr/include/stdio.h /usr/include/sys/types.h
f77: /usr/include/sys/stat.h /usr/include/ctype.h /usr/include/signal.h
f77: /usr/include/machine/trap.h /usr/include/sys/time.h /usr/include/time.h
f77: /usr/include/sys/resource.h /usr/include/sys/wait.h
f77: /usr/include/machine/machparam.h ../include/defines.h
f77: ../include/machdefs.h drivedefs.h ../include/version.h
f77: /usr/include/errno.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
