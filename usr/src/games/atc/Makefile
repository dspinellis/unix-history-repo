#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.1	(Berkeley)	%G%
#

CFLAGS=	-O -DBSD -DDEST=\"${DESTDIR}/usr/games/lib/atc/\"
YFLAGS=	-d
LIBC=	/lib/libc.a
LIBS=	-ll -lm -lcurses -ltermcap
SRCS=	extern.c grammar.c graphics.c input.c lex.c list.c log.c \
	main.c tunable.c update.c
OBJS=	extern.o grammar.o input.o lex.o list.o log.o main.o tunable.o \
	graphics.o update.o

all: atc

atc: ${OBJS} ${LIBC}
	${CC} ${CFLAGS} ${OBJS} ${LIBS} -o $@

clean: FRC
	rm -f ${OBJS} core atc grammar.c y.tab.h y.tab.c lex.yy.c lex.c

depend: lex.c grammar.c FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	-[ -d ${DESTDIR}/usr/games/lib/atc ] || mkdir ${DESTDIR}/usr/games/lib/atc
	chmod 700 ${DESTDIR}/usr/games/lib/atc
	chown games.bin ${DESTDIR}/usr/games/lib/atc
	(cd games; install -c -o games -g bin -m 600 * ${DESTDIR}/usr/games/lib/atc)
	install -s -o games -g bin -m 4700 atc ${DESTDIR}/usr/games/hide/atc
	(cd ${DESTDIR}/usr/games; rm -f atc; ln -s dm atc; chown games.bin atc)

lint: lex.c grammar.c FRC
	lint ${CFLAGS} ${SRCS}

tags: lex.c grammar.c FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

extern.o: extern.c include.h /usr/include/stdio.h /usr/include/ctype.h
extern.o: /usr/include/pwd.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
extern.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
extern.o: /usr/include/sys/time.h /usr/include/time.h /usr/include/sys/file.h
extern.o: /usr/include/signal.h /usr/include/machine/trap.h /usr/include/math.h
extern.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h def.h
extern.o: struct.h extern.h tunable.h
grammar.o: grammar.c include.h /usr/include/stdio.h /usr/include/ctype.h
grammar.o: /usr/include/pwd.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
grammar.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
grammar.o: /usr/include/sys/time.h /usr/include/time.h /usr/include/sys/file.h
grammar.o: /usr/include/signal.h /usr/include/machine/trap.h
grammar.o: /usr/include/math.h /usr/include/curses.h /usr/include/stdio.h
grammar.o: /usr/include/sgtty.h def.h struct.h extern.h tunable.h
graphics.o: graphics.c include.h /usr/include/stdio.h /usr/include/ctype.h
graphics.o: /usr/include/pwd.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
graphics.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
graphics.o: /usr/include/sys/time.h /usr/include/time.h /usr/include/sys/file.h
graphics.o: /usr/include/signal.h /usr/include/machine/trap.h
graphics.o: /usr/include/math.h /usr/include/curses.h /usr/include/stdio.h
graphics.o: /usr/include/sgtty.h def.h struct.h extern.h tunable.h
input.o: input.c include.h /usr/include/stdio.h /usr/include/ctype.h
input.o: /usr/include/pwd.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
input.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
input.o: /usr/include/sys/time.h /usr/include/time.h /usr/include/sys/file.h
input.o: /usr/include/signal.h /usr/include/machine/trap.h /usr/include/math.h
input.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h def.h
input.o: struct.h extern.h tunable.h
lex.o: lex.c /usr/include/stdio.h y.tab.h
list.o: list.c include.h /usr/include/stdio.h /usr/include/ctype.h
list.o: /usr/include/pwd.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
list.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
list.o: /usr/include/sys/time.h /usr/include/time.h /usr/include/sys/file.h
list.o: /usr/include/signal.h /usr/include/machine/trap.h /usr/include/math.h
list.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h def.h
list.o: struct.h extern.h tunable.h
log.o: log.c include.h /usr/include/stdio.h /usr/include/ctype.h
log.o: /usr/include/pwd.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
log.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
log.o: /usr/include/sys/time.h /usr/include/time.h /usr/include/sys/file.h
log.o: /usr/include/signal.h /usr/include/machine/trap.h /usr/include/math.h
log.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h def.h
log.o: struct.h extern.h tunable.h
main.o: main.c include.h /usr/include/stdio.h /usr/include/ctype.h
main.o: /usr/include/pwd.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
main.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
main.o: /usr/include/sys/time.h /usr/include/time.h /usr/include/sys/file.h
main.o: /usr/include/signal.h /usr/include/machine/trap.h /usr/include/math.h
main.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h def.h
main.o: struct.h extern.h tunable.h
tunable.o: tunable.c
update.o: update.c include.h /usr/include/stdio.h /usr/include/ctype.h
update.o: /usr/include/pwd.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
update.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
update.o: /usr/include/sys/time.h /usr/include/time.h /usr/include/sys/file.h
update.o: /usr/include/signal.h /usr/include/machine/trap.h /usr/include/math.h
update.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h def.h
update.o: struct.h extern.h tunable.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
