#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.4	(Berkeley)	%G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
LIBS=	-lcurses -ltermlib
SRCS=	comp.c end.c extern.c init.c mille.c misc.c move.c print.c \
	roll.c save.c table.c types.c varpush.c
MSRCS=	comp.c end.c extern.c init.c mille.c misc.c move.c print.c \
	roll.c save.c types.c varpush.c
MOBJS=	comp.o end.o extern.o init.o mille.o misc.o move.o print.o \
	roll.o save.o types.o varpush.o
TSRCS=	table.c extern.c
TOBJS=	table.o extern.o

all: mille

mille: ${MOBJS} ${LIBC}
	${CC} ${CFLAGS} -o $@ ${MOBJS} ${LIBS}

table: ${TOBJS} ${LIBC}
	${CC} ${CFLAGS} -o $@ ${TOBJS}

clean: FRC
	rm -f ${OBJS} ${TOBJS} core mille table

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 mille ${DESTDIR}/usr/games/mille

lint: FRC
	lint ${CFLAGS} ${SRCS}
	lint ${CFLAGS} ${TSRCS}

tags: FRC
	ctags ${SRCS}
	ctags -a ${TSRCS}
	sort -o tags tags

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

comp.o: comp.c mille.h /usr/include/ctype.h /usr/include/curses.h
comp.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
comp.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
end.o: end.c mille.h /usr/include/ctype.h /usr/include/curses.h
end.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
end.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
extern.o: extern.c mille.h /usr/include/ctype.h /usr/include/curses.h
extern.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
extern.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
init.o: init.c mille.h /usr/include/ctype.h /usr/include/curses.h
init.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
init.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
mille.o: mille.c mille.h /usr/include/ctype.h /usr/include/curses.h
mille.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
mille.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
mille.o: /usr/include/signal.h
misc.o: misc.c mille.h /usr/include/ctype.h /usr/include/curses.h
misc.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
misc.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
misc.o: /usr/include/sys/file.h
move.o: move.c mille.h /usr/include/ctype.h /usr/include/curses.h
move.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
move.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
print.o: print.c mille.h /usr/include/ctype.h /usr/include/curses.h
print.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
print.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
roll.o: roll.c mille.h /usr/include/ctype.h /usr/include/curses.h
roll.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
roll.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
save.o: save.c mille.h /usr/include/ctype.h /usr/include/curses.h
save.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
save.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
save.o: /usr/include/sys/types.h /usr/include/sys/stat.h
table.o: table.c mille.h /usr/include/ctype.h /usr/include/curses.h
table.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
table.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
types.o: types.c mille.h /usr/include/ctype.h /usr/include/curses.h
types.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
types.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
varpush.o: varpush.c mille.h /usr/include/ctype.h /usr/include/curses.h
varpush.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
varpush.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
