#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation,
# advertising materials, and other materials related to such
# distribution and use acknowledge that the software was developed
# by the University of California, Berkeley.  The name of the
# University may not be used to endorse or promote products derived
# from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	5.7 (Berkeley) 6/18/88
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
	rm -f ${OBJS} ${TOBJS} ${MOBJS} core mille table

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o games -g bin -m 700 mille ${DESTDIR}/usr/games/hide
	(cd ${DESTDIR}/usr/games; rm -f mille; ln -s dm mille; chown games.bin mille)

lint: FRC
	lint ${CFLAGS} ${MSRCS}
	lint ${CFLAGS} ${TSRCS}

tags: FRC
	ctags ${MSRCS}
	ctags -a ${TSRCS}
	sort -o tags tags

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

comp.o: comp.c mille.h /usr/include/sys/types.h /usr/include/ctype.h
comp.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
comp.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
comp.o: /usr/include/sys/ttydev.h /usr/include/strings.h
end.o: end.c mille.h /usr/include/sys/types.h /usr/include/ctype.h
end.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
end.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
end.o: /usr/include/sys/ttydev.h /usr/include/strings.h
extern.o: extern.c mille.h /usr/include/sys/types.h /usr/include/ctype.h
extern.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
extern.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
extern.o: /usr/include/sys/ttydev.h /usr/include/strings.h
init.o: init.c mille.h /usr/include/sys/types.h /usr/include/ctype.h
init.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
init.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
init.o: /usr/include/sys/ttydev.h /usr/include/strings.h
mille.o: mille.c mille.h /usr/include/sys/types.h /usr/include/ctype.h
mille.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
mille.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
mille.o: /usr/include/sys/ttydev.h /usr/include/strings.h /usr/include/signal.h
mille.o: /usr/include/machine/trap.h
misc.o: misc.c mille.h /usr/include/sys/types.h /usr/include/ctype.h
misc.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
misc.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
misc.o: /usr/include/sys/ttydev.h /usr/include/strings.h
misc.o: /usr/include/sys/file.h
move.o: move.c mille.h /usr/include/sys/types.h /usr/include/ctype.h
move.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
move.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
move.o: /usr/include/sys/ttydev.h /usr/include/strings.h
print.o: print.c mille.h /usr/include/sys/types.h /usr/include/ctype.h
print.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
print.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
print.o: /usr/include/sys/ttydev.h /usr/include/strings.h
roll.o: roll.c mille.h /usr/include/sys/types.h /usr/include/ctype.h
roll.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
roll.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
roll.o: /usr/include/sys/ttydev.h /usr/include/strings.h
save.o: save.c mille.h /usr/include/sys/types.h /usr/include/ctype.h
save.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
save.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
save.o: /usr/include/sys/ttydev.h /usr/include/strings.h
save.o: /usr/include/sys/types.h /usr/include/sys/stat.h
table.o: table.c mille.h /usr/include/sys/types.h /usr/include/ctype.h
table.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
table.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
table.o: /usr/include/sys/ttydev.h /usr/include/strings.h
types.o: types.c mille.h /usr/include/sys/types.h /usr/include/ctype.h
types.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
types.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
types.o: /usr/include/sys/ttydev.h /usr/include/strings.h
varpush.o: varpush.c mille.h /usr/include/sys/types.h /usr/include/ctype.h
varpush.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
varpush.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
varpush.o: /usr/include/sys/ttydev.h /usr/include/strings.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
