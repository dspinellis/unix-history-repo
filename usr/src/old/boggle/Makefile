#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.8	(Berkeley)	%G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	comp.c boggle.c

all: boggle bogdict

bogdict: comp sfile
	-if [ -f /usr/local/lib/w2a ]; \
		then dict=/usr/local/lib/w2a; \
		else dict=/usr/dict/words; fi; \
	sed -f sfile $$dict | uniq | ./comp >bogdict

boggle: ${LIBC}
	${CC} -o $@ ${CFLAGS} $@.c

comp: ${LIBC}
	${CC} -o $@ ${CFLAGS} $@.c

clean: FRC
	rm -f ${OBJS} core comp boggle bogdict

depend: FRC
	mkdep -p ${CFLAGS} ${SRCS}

install: FRC
	install -s -o games -g bin -m 4700 boggle ${DESTDIR}/usr/games/hide/boggle
	install -o games -g bin -m 400 bogdict ${DESTDIR}/usr/games/lib/bogdict
	(cd ${DESTDIR}/usr/games; rm -f boggle; ln -s dm boggle; chown games.bin boggle)

lint: FRC
	lint ${CFLAGS} comp.c
	lint ${CFLAGS} boggle.c

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

comp: comp.c /usr/include/stdio.h
boggle: boggle.c /usr/include/ctype.h /usr/include/errno.h
boggle: /usr/include/setjmp.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
boggle: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
boggle: /usr/include/signal.h /usr/include/machine/trap.h /usr/include/stdio.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
