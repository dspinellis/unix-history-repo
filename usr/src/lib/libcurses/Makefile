#
# cursor package maker
#
HEADERS=curses.h unctrl.h cr_ex.h
CFILES=	box.c clear.c initscr.c endwin.c mvprintw.c mvscanw.c mvwin.c \
	newwin.c overlay.c overwrite.c printw.c scanw.c refresh.c \
	touchwin.c erase.c clrtobot.c clrtoeol.c cr_put.c cr_tty.c \
	longname.c delwin.c insertln.c deleteln.c scroll.c getstr.c \
	getch.c addstr.c addch.c move.c curses.c unctrl.c standout.c

OBJS=	box.o clear.o initscr.o endwin.o mvprintw.o mvscanw.o mvwin.o \
	newwin.o overlay.o overwrite.o printw.o scanw.o refresh.o \
	touchwin.o erase.o clrtobot.o clrtoeol.o cr_put.o cr_tty.o \
	longname.o delwin.o insertln.o deleteln.o scroll.o getstr.o \
	getch.o addstr.o addch.o move.o curses.o unctrl.o standout.o

POBJS=	box.po clear.po initscr.po endwin.po mvprintw.po mvscanw.po mvwin.po \
	newwin.po overlay.po overwrite.po printw.po scanw.po refresh.po \
	touchwin.po erase.po clrtobot.po clrtoeol.po cr_put.po cr_tty.po \
	longname.po delwin.po insertln.po deleteln.po scroll.po getstr.po \
	getch.po addstr.po addch.po move.po curses.po unctrl.po standout.po

CTAGS=	ctags
CC=	cc
LINT=	lint
AR=	ar
RM=	rm
LN=	ln
CFL=
CFLAGS=	-O ${CFL}
.SUFFIXES: .po

.c.po:
	ln $*.c x.c
	${CC} ${CFLAGS} -p -c x.c
	mv x.o $*.po ; rm x.c

crlib: ${OBJS}
	${AR} ruv crlib ${OBJS}
	ranlib crlib

pcrlib: ${POBJS}
	${AR} ruv pcrlib ${POBJS}
	ranlib pcrlib

test:	crlib test.o
	${CC} ${CFLAGS} -o test test.o crlib -ltermlib

a.out:	crlib cat.o
	${CC} ${CFLAGS} cat.o crlib -ltermlib

cat.o:	curses.h
	${CC} -c ${CFLAGS} cat.c

new:	cleanup ctags
	${CC} -c ${CFLAGS} ${CFILES}
	${AR} qv crlib ${OBJS}
	${RM} /ya/staff/arnold/=eye/crlib /ya/staff/arnold/=hacks/crlib \
	      /ya/staff/arnold/=eye/curses.h /ya/staff/arnold/=hacks/curses.h

clean:
	rm ${OBJS} crlib

ctags:
	${CTAGS} ${CFILES} curses.h cr_ex.h

lint:
	${LINT} -hxb ${CFL} ${CFILES} -lcurses > lint.out

lpr:
	-pr curses.h ${CFILES} | lpr
	-lpq

tp:
	tp crm0 Makefile tags ${HEADERS} curses.ext ${CFILES} llib-lcurses

tar:
	tar crvf curses.tar Makefile tags ${HEADERS} curses.ext ${CFILES} llib-lcurses

llib-lcurses.ln: llib-lcurses
	-(/lib/cpp -C -Dlint llib-lcurses | /usr/lib/lpass1 > llib-lcurses.ln ) 2>&1 | grep -v warning

install:
	install -c crlib ${DESTDIR}/usr/lib/libcurses.a
	ranlib ${DESTDIR}/usr/lib/libcurses.a
