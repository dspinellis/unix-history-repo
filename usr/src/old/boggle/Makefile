#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.7	(Berkeley)	%G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	comp.c boggle.c
CSRCS=	comp.c
COBJS=	comp.o
BSRCS=	boggle.c
BOBJS=	boggle.o

all: boggle bogdict

bogdict: comp sfile
	-if [ -f /usr/local/lib/w2a ]; \
		then dict=/usr/local/lib/w2a; \
		else dict=/usr/dict/words; fi; \
	sed -f sfile $$dict | uniq | ./comp >bogdict

boggle:	${BOBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${BOBJS}

comp:	${COBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${COBJS}

clean: FRC
	rm -f ${OBJS} core comp boggle bogdict

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 boggle ${DESTDIR}/usr/games/boggle
	install -o bin -g bin -m 444 bogdict ${DESTDIR}/usr/games/bogdict

lint: FRC
	lint ${CFLAGS} ${CSRCS}
	lint ${CFLAGS} ${BSRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

comp.o: comp.c /usr/include/stdio.h
boggle.o: boggle.c /usr/include/ctype.h /usr/include/errno.h
boggle.o: /usr/include/setjmp.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
boggle.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
boggle.o: /usr/include/signal.h /usr/include/stdio.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
