#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.2	(Berkeley)	%G%
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
	install -s -o bin -g bin -m 755 apropos ${DESTDIR}/usr/ucb/apropos
	install -s -o bin -g bin -m 755 man ${DESTDIR}/usr/ucb/man

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
