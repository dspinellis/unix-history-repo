#	Makefile	1.17	83/07/02
#
# curses package
#
CFILES=	box.c clear.c initscr.c endwin.c mvprintw.c mvscanw.c mvwin.c \
	newwin.c overlay.c overwrite.c printw.c scanw.c refresh.c \
	touchwin.c erase.c clrtobot.c clrtoeol.c cr_put.c cr_tty.c \
	longname.c delwin.c insertln.c deleteln.c scroll.c getstr.c \
	getch.c addstr.c addch.c move.c curses.c unctrl.c standout.c \
	tstp.c insch.c delch.c
OBJS=	addch.o addstr.o box.o clear.o clrtobot.o clrtoeol.o cr_put.o \
	cr_tty.o curses.o delch.o deleteln.o delwin.o endwin.o erase.o \
	getch.o getstr.o initscr.o insch.o insertln.o longname.o move.o \
	mvprintw.o mvscanw.o mvwin.o newwin.o overlay.o overwrite.o \
	printw.o refresh.o scanw.o scroll.o standout.o touchwin.o tstp.o \
	unctrl.o
CTAGS=	ctags
CFLAGS=	-O

.c.o:
	${CC} -c -p ${CFLAGS} $*.c
	ld -x -r $*.o
	mv a.out profiled/$*.o
	${CC} -c ${CFLAGS} $*.c
	ld -x -r $*.o
	mv a.out $*.o

libcurses libcurses_p: ${OBJS}
	@echo building profiled libcurses
	@cd profiled; ar cr ../libcurses_p ${OBJS}
	@echo building normal libcurses
	@ar cr libcurses ${OBJS}

install: libcurses libcurses_p
	install -m 644 libcurses ${DESTDIR}/usr/lib/libcurses.a
	ranlib ${DESTDIR}/usr/lib/libcurses.a
	install -m 644 libcurses_p ${DESTDIR}/usr/lib/libcurses_p.a
	ranlib ${DESTDIR}/usr/lib/libcurses_p.a

clean:
	rm -f ${OBJS} profiled/*.o a.out core test errs libcurses libcurses_p

ctags:
	${CTAGS} ${CFILES} curses.h

lint:
	lint -hxb ${CFILES} -lcurses > lint.out

test:	libcurses test.o
	${CC} ${LDFLAGS} ${CFLAGS} -o test test.o crlib -ltermlib

cat.o:	curses.h
	${CC} -c ${CFLAGS} cat.c
