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
#	@(#)Makefile	5.4 (Berkeley) %G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	addbytes.c addch.c addstr.c box.c clear.c clrtobot.c clrtoeol.c \
	cr_put.c cr_tty.c curses.c delch.c deleteln.c delwin.c endwin.c \
	erase.c fullname.c getch.c getstr.c idlok.c id_subwins.c initscr.c \
	insch.c insertln.c longname.c move.c mvprintw.c mvscanw.c mvwin.c \
	newwin.c overlay.c overwrite.c printw.c putchar.c refresh.c scanw.c \
	scroll.c toucholap.c standout.c touchwin.c tstp.c unctrl.c
OBJS=	addbytes.o addch.o addstr.o box.o clear.o clrtobot.o clrtoeol.o \
	cr_put.o cr_tty.o curses.o delch.o deleteln.o delwin.o endwin.o \
	erase.o fullname.o getch.o getstr.o idlok.o id_subwins.o initscr.o \
	insch.o insertln.o longname.o move.o mvprintw.o mvscanw.o mvwin.o \
	newwin.o overlay.o overwrite.o printw.o putchar.o refresh.o scanw.o \
	scroll.o toucholap.o standout.o touchwin.o tstp.o unctrl.o

.c.o:
	@${CC} -c -pg ${CFLAGS} $*.c
	@ld -x -o profiled/$*.o -r $*.o
	${CC} ${CFLAGS} -c $*.c
	@ld -x -r $*.o
	@mv a.out $*.o

all: libcurses libcurses_p

libcurses libcurses_p: ${OBJS}
	@echo building profiled libcurses
	@cd profiled; ar cu ../libcurses_p ${OBJS}
	ranlib libcurses_p
	@echo building normal libcurses
	@ar cu libcurses ${OBJS}
	ranlib libcurses

clean: FRC
	rm -f ${OBJS} profiled/*.o libcurses libcurses_p

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -o bin -g bin -m 644 libcurses ${DESTDIR}/usr/lib/libcurses.a
	ranlib ${DESTDIR}/usr/lib/libcurses.a
	install -o bin -g bin -m 644 libcurses_p ${DESTDIR}/usr/lib/libcurses_p.a
	ranlib ${DESTDIR}/usr/lib/libcurses_p.a

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

addbytes.o: addbytes.c curses.ext curses.h /usr/include/stdio.h
addbytes.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
addbytes.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
addch.o: addch.c curses.ext curses.h /usr/include/stdio.h /usr/include/sgtty.h
addch.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
addch.o: /usr/include/sys/ttydev.h
addstr.o: addstr.c curses.ext curses.h /usr/include/stdio.h
addstr.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
addstr.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
box.o: box.c curses.ext curses.h /usr/include/stdio.h /usr/include/sgtty.h
box.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
box.o: /usr/include/sys/ttydev.h
clear.o: clear.c curses.ext curses.h /usr/include/stdio.h /usr/include/sgtty.h
clear.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
clear.o: /usr/include/sys/ttydev.h
clrtobot.o: clrtobot.c curses.ext curses.h /usr/include/stdio.h
clrtobot.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
clrtobot.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
clrtoeol.o: clrtoeol.c curses.ext curses.h /usr/include/stdio.h
clrtoeol.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
clrtoeol.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
cr_put.o: cr_put.c curses.ext curses.h /usr/include/stdio.h
cr_put.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
cr_put.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
cr_tty.o: cr_tty.c curses.ext curses.h /usr/include/stdio.h
cr_tty.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
cr_tty.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
curses.o: curses.c curses.h /usr/include/stdio.h /usr/include/sgtty.h
curses.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
curses.o: /usr/include/sys/ttydev.h
delch.o: delch.c curses.ext curses.h /usr/include/stdio.h /usr/include/sgtty.h
delch.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
delch.o: /usr/include/sys/ttydev.h
deleteln.o: deleteln.c curses.ext curses.h /usr/include/stdio.h
deleteln.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
deleteln.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
delwin.o: delwin.c curses.ext curses.h /usr/include/stdio.h
delwin.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
delwin.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
endwin.o: endwin.c curses.ext curses.h /usr/include/stdio.h
endwin.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
endwin.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
erase.o: erase.c curses.ext curses.h /usr/include/stdio.h /usr/include/sgtty.h
erase.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
erase.o: /usr/include/sys/ttydev.h
fullname.o: fullname.c
getch.o: getch.c curses.ext curses.h /usr/include/stdio.h /usr/include/sgtty.h
getch.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
getch.o: /usr/include/sys/ttydev.h
getstr.o: getstr.c curses.ext curses.h /usr/include/stdio.h
getstr.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
getstr.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
idlok.o: idlok.c curses.ext curses.h /usr/include/stdio.h /usr/include/sgtty.h
idlok.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
idlok.o: /usr/include/sys/ttydev.h
id_subwins.o: id_subwins.c curses.ext curses.h /usr/include/stdio.h
id_subwins.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
id_subwins.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
initscr.o: initscr.c curses.ext curses.h /usr/include/stdio.h
initscr.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
initscr.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
initscr.o: /usr/include/signal.h
insch.o: insch.c curses.ext curses.h /usr/include/stdio.h /usr/include/sgtty.h
insch.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
insch.o: /usr/include/sys/ttydev.h
insertln.o: insertln.c curses.ext curses.h /usr/include/stdio.h
insertln.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
insertln.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
longname.o: longname.c
move.o: move.c curses.ext curses.h /usr/include/stdio.h /usr/include/sgtty.h
move.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
move.o: /usr/include/sys/ttydev.h
mvprintw.o: mvprintw.c curses.ext curses.h /usr/include/stdio.h
mvprintw.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
mvprintw.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
mvscanw.o: mvscanw.c curses.ext curses.h /usr/include/stdio.h
mvscanw.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
mvscanw.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
mvwin.o: mvwin.c curses.ext curses.h /usr/include/stdio.h /usr/include/sgtty.h
mvwin.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
mvwin.o: /usr/include/sys/ttydev.h
newwin.o: newwin.c curses.ext curses.h /usr/include/stdio.h
newwin.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
newwin.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
overlay.o: overlay.c curses.ext curses.h /usr/include/stdio.h
overlay.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
overlay.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
overlay.o: /usr/include/ctype.h
overwrite.o: overwrite.c curses.ext curses.h /usr/include/stdio.h
overwrite.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
overwrite.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
overwrite.o: /usr/include/ctype.h
printw.o: printw.c curses.ext curses.h /usr/include/stdio.h
printw.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
printw.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
putchar.o: putchar.c curses.ext curses.h /usr/include/stdio.h
putchar.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
putchar.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
refresh.o: refresh.c curses.ext curses.h /usr/include/stdio.h
refresh.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
refresh.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
scanw.o: scanw.c curses.ext curses.h /usr/include/stdio.h /usr/include/sgtty.h
scanw.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
scanw.o: /usr/include/sys/ttydev.h
scroll.o: scroll.c curses.ext curses.h /usr/include/stdio.h
scroll.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
scroll.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
toucholap.o: toucholap.c curses.ext curses.h /usr/include/stdio.h
toucholap.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
toucholap.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
standout.o: standout.c curses.ext curses.h /usr/include/stdio.h
standout.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
standout.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
touchwin.o: touchwin.c curses.ext curses.h /usr/include/stdio.h
touchwin.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
touchwin.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
tstp.o: tstp.c /usr/include/signal.h curses.ext curses.h /usr/include/stdio.h
tstp.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
tstp.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
unctrl.o: unctrl.c

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
