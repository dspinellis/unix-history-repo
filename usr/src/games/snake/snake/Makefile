#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.3	(Berkeley)	%G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
LIBS=	-lm -ltermcap
SRCS1=	snake.c move.c
OBJS1=	snake.o move.o
SRCS2=	snscore.c
OBJS2=	snscore.o
SRCS3=	busy.c
OBJS3=	busy.o

all:	snake snscore

snake:	${OBJS1} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS1} ${LIBS}

snscore: ${OBJS2} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS2}

busy:	${OBJS3} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS3}

clean: FRC
	rm -f ${OBJS1} ${OBJS2} ${OBJS3} core snake snscore busy

depend: FRC
	mkdep ${CFLAGS} ${SRCS1} ${SRCS2} ${SRCS3}

install: FRC
	install -s -o daemon -g bin -m 4755 snake ${DESTDIR}/usr/games/snake
	install -s -o daemon -g bin -m 755 snscore ${DESTDIR}/usr/games/snscore
	install -c -o daemon -g bin -m 644 /dev/null ${DESTDIR}/usr/games/lib/snakerawscores
#	install -s -o daemon -g bin -m 755 busy ${DESTDIR}/usr/games/busy

lint: FRC
	lint ${CFLAGS} ${SRCS1}
	lint ${CFLAGS} ${SRCS2}
	lint ${CFLAGS} ${SRCS3}

tags: FRC
	ctags ${SRCS1} ${SRCS2} ${SRCS3}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

snake.o: snake.c snake.h /usr/include/stdio.h /usr/include/assert.h
snake.o: /usr/include/sys/types.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
snake.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
snake.o: /usr/include/signal.h /usr/include/math.h /usr/include/pwd.h
move.o: move.c snake.h /usr/include/stdio.h /usr/include/assert.h
move.o: /usr/include/sys/types.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
move.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
move.o: /usr/include/signal.h /usr/include/math.h
snscore.o: snscore.c /usr/include/stdio.h /usr/include/pwd.h
busy.o: busy.c /usr/include/stdio.h /usr/include/sys/types.h
busy.o: /usr/include/a.out.h /usr/include/sys/exec.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
