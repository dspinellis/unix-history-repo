#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.1 (Berkeley) %G%
#
# curses package
#
CFILES=	addch.c addstr.c box.c clear.c clrtobot.c clrtoeol.c cr_put.c \
	cr_tty.c curses.c delch.c deleteln.c delwin.c endwin.c erase.c \
	fullname.c getch.c getstr.c idlok.c id_subwins.c initscr.c insch.c \
	insertln.c longname.c move.c mvprintw.c mvscanw.c mvwin.c newwin.c \
	overlay.c overwrite.c printw.c putchar.c refresh.c scanw.c scroll.c \
	toucholap.c standout.c touchwin.c tstp.c unctrl.c
OBJS=	addch.o addstr.o box.o clear.o clrtobot.o clrtoeol.o cr_put.o \
	cr_tty.o curses.o delch.o deleteln.o delwin.o endwin.o erase.o \
	fullname.o getch.o getstr.o idlok.o id_subwins.o initscr.o insch.o \
	insertln.o longname.o move.o mvprintw.o mvscanw.o mvwin.o newwin.o \
	overlay.o overwrite.o printw.o putchar.o refresh.o scanw.o scroll.o \
	toucholap.o standout.o touchwin.o tstp.o unctrl.o

CTAGS=	ctags
DEFS=
CFLAGS=	-O $(DEFS)
TAGSFILE=tags

.c.o:
	${CC} -c -pg ${CFLAGS} $*.c
	ld -x -r $*.o
	mv a.out profiled/$*.o
	${CC} -c ${CFLAGS} $*.c
	ld -x -r $*.o
	mv a.out $*.o

libcurses libcurses_p: ${OBJS}
	@echo building profiled libcurses
	@cd profiled; ar cu ../libcurses_p ${OBJS}
	ranlib libcurses_p
	@echo building normal libcurses
	@ar cu libcurses ${OBJS}
	ranlib libcurses

install: libcurses libcurses_p
	install -m 644 libcurses ${DESTDIR}/usr/lib/libcurses.a
	ranlib ${DESTDIR}/usr/lib/libcurses.a
	install -m 644 libcurses_p ${DESTDIR}/usr/lib/libcurses_p.a
	ranlib ${DESTDIR}/usr/lib/libcurses_p.a

tags:
	cwd=`pwd`; \
	for i in ${CFILES}; do \
		ctags -a -f ${TAGSFILE} $$cwd/$$i; \
	done

clean:
	rm -f ${OBJS} profiled/*.o a.out core test errs \
	    libcurses libcurses_p tags

ctags:
	${CTAGS} ${CFILES} curses.h

lint:
	lint -hxb ${CFILES} -lcurses > lint.out

test:	libcurses test.o
	${CC} ${LDFLAGS} ${CFLAGS} -o test test.o libcurses -ltermlib

test.o:	test.c
	${CC} ${CFLAGS} -c test.c

ar:
	ar crv curses.ar ${CFILES} curses.h curses.ext Makefile
