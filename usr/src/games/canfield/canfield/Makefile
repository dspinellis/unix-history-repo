#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.1	(Berkeley)	%G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
CSRCS=	canfield.c
CFSRCS=	cfscores.c

all: canfield cfscores

canfield: ${LIBC}
	${CC} -o $@ ${CFLAGS} ${CSRCS} -lcurses -ltermcap

cfscores: ${LIBC}
	${CC} -o $@ ${CFLAGS} ${CFSRCS}

clean: FRC
	rm -f core canfield cfscores

depend: FRC
	mkdep -p ${CFLAGS} ${CSRCS} ${CFSRCS}

install: FRC
	install -s -o games -g bin -m 700 canfield ${DESTDIR}/usr/games/hide
	install -s -o games -g bin -m 700 cfscores ${DESTDIR}/usr/games/hide
	(cd ${DESTDIR}/usr/games; rm -f canfield; ln -s dm canfield; chown games.bin canfield)
	(cd ${DESTDIR}/usr/games; rm -f cfscores; ln -s dm cfscores; chown games.bin cfscores)
	-[ -f ${DESTDIR}/usr/games/lib/cfscores ] || install -c -o games -g bin -m 600 /dev/null ${DESTDIR}/usr/games/lib/cfscores

lint: FRC
	lint ${CFLAGS} ${CSRCS}
	lint ${CFLAGS} ${CFSRCS}

tags: FRC
	ctags ${CSRCS} ${CFSRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

canfield: canfield.c /usr/include/curses.h /usr/include/stdio.h
canfield: /usr/include/sgtty.h /usr/include/sys/ioctl.h
canfield: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
canfield: /usr/include/ctype.h /usr/include/signal.h
canfield: /usr/include/machine/trap.h /usr/include/sys/types.h
cfscores: cfscores.c /usr/include/pwd.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
