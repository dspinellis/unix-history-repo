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
#	@(#)Makefile	5.3 (Berkeley) %G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
ASRCS=	apropos.c
AOBJS=	apropos.o
MSRCS=	man.c
MOBJS=	man.o

all: man apropos

apropos: ${AOBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${AOBJS}

man:	${MOBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${MOBJS}

clean: FRC
	rm -f ${AOBJS} ${MOBJS} core apropos man

depend: FRC
	mkdep ${CFLAGS} ${ASRCS} ${MSRCS}

install: FRC
	rm -f ${DESTDIR}/usr/ucb/whatis ${DESTDIR}/usr/ucb/apropos
	install -s -o bin -g bin -m 755 apropos ${DESTDIR}/usr/ucb/apropos
	install -s -o bin -g bin -m 755 man ${DESTDIR}/usr/ucb/man
	ln ${DESTDIR}/usr/ucb/apropos ${DESTDIR}/usr/ucb/whatis

lint: FRC
	lint ${CFLAGS} ${ASRCS}
	lint ${CFLAGS} ${MSRCS}

tags: FRC
	ctags ${ASRCS}
	ctags ${MSRCS}
	sort -o tags tags

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

apropos.o: apropos.c /usr/include/sys/param.h /usr/include/sys/types.h
apropos.o: /usr/include/signal.h /usr/include/machine/machparam.h
apropos.o: /usr/include/stdio.h /usr/include/ctype.h
man.o: man.c /usr/include/sys/param.h /usr/include/sys/types.h
man.o: /usr/include/signal.h /usr/include/machine/machparam.h
man.o: /usr/include/sys/file.h /usr/include/stdio.h /usr/include/ctype.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
