#
# cursor package maker
#
# @(#)Makefile	1.11 (Berkeley) %G%
#
HEADERS=curses.h unctrl.h cr_ex.h curses.ext
CFILES=	box.c clear.c initscr.c endwin.c mvprintw.c mvscanw.c mvwin.c \
	newwin.c overlay.c overwrite.c printw.c scanw.c refresh.c \
	touchwin.c erase.c clrtobot.c clrtoeol.c cr_put.c cr_tty.c \
	longname.c delwin.c insertln.c deleteln.c scroll.c getstr.c \
	getch.c addstr.c addch.c move.c curses.c unctrl.c standout.c \
	tstp.c insch.c delch.c

OBJS=	box.o clear.o initscr.o endwin.o mvprintw.o mvscanw.o mvwin.o \
	newwin.o overlay.o overwrite.o printw.o scanw.o refresh.o \
	touchwin.o erase.o clrtobot.o clrtoeol.o cr_put.o cr_tty.o \
	longname.o delwin.o insertln.o deleteln.o scroll.o getstr.o \
	getch.o addstr.o addch.o move.o curses.o unctrl.o standout.o \
	tstp.o insch.o delch.o

POBJS=	box.p clear.p initscr.p endwin.p mvprintw.p mvscanw.p mvwin.p \
	newwin.p overlay.p overwrite.p printw.p scanw.p refresh.p \
	touchwin.p erase.p clrtobot.p clrtoeol.p cr_put.p cr_tty.p \
	longname.p delwin.p insertln.p deleteln.p scroll.p getstr.p \
	getch.p addstr.p addch.p move.p curses.p unctrl.p standout.p \
	tstp.p insch.p delch.p


CTAGS=	ctags
CC=	cc
LINT=	lint
LPASS1=	/usr/lib/lint/lint1
AR=	ar
RM=	rm
LN=	ln
CFL=	
CFLAGS=	-O ${CFL}
LDFLAGS=-n
.SUFFIXES: .p

.c.p:
	rm -f x.c
	ln $*.c x.c
	${CC} ${CFLAGS} -p -c x.c
	mv x.o $*.p

libcurses: ${HEADERS} crlib

crlib: ${OBJS}
	${AR} rv crlib $?
	ranlib crlib

pcrlib: ${POBJS}
	rm -f x.c
	${AR} rv pcrlib $?
	ranlib pcrlib

test:	crlib test.o
	${CC} ${LDFLAGS} ${CFLAGS} -o test test.o crlib -ltermlib

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
	tp crm0 Makefile tags ${HEADERS} ${CFILES} llib-lcurses

tar:
	tar crvf curses.tar Makefile tags ${HEADERS} ${CFILES} llib-lcurses

llib-lcurses.ln: llib-lcurses
	-(/lib/cpp -C -Dlint llib-lcurses | ${LPASS1} > llib-lcurses.ln ) 2>&1 | grep -v warning

install:
	install -c crlib ${DESTDIR}/usr/lib/libcurses.a
	ranlib ${DESTDIR}/usr/lib/libcurses.a

source: ${HEADERS} ${CFILES}

addch.c: SCCS/s.addch.c ; sccs get addch.c
addstr.c: SCCS/s.addstr.c ; sccs get addstr.c
box.c: SCCS/s.box.c ; sccs get box.c
clear.c: SCCS/s.clear.c ; sccs get clear.c
clrtobot.c: SCCS/s.clrtobot.c ; sccs get clrtobot.c
clrtoeol.c: SCCS/s.clrtoeol.c ; sccs get clrtoeol.c
cr_ex.h: SCCS/s.cr_ex.h ; sccs get cr_ex.h
cr_put.c: SCCS/s.cr_put.c ; sccs get cr_put.c
cr_tty.c: SCCS/s.cr_tty.c ; sccs get cr_tty.c
curses.c: SCCS/s.curses.c ; sccs get curses.c
curses.h: SCCS/s.curses.h ; sccs get curses.h
delch.c: SCCS/s.delch.c ; sccs get delch.c
deleteln.c: SCCS/s.deleteln.c ; sccs get deleteln.c
delwin.c: SCCS/s.delwin.c ; sccs get delwin.c
endwin.c: SCCS/s.endwin.c ; sccs get endwin.c
erase.c: SCCS/s.erase.c ; sccs get erase.c
getch.c: SCCS/s.getch.c ; sccs get getch.c
getstr.c: SCCS/s.getstr.c ; sccs get getstr.c
initscr.c: SCCS/s.initscr.c ; sccs get initscr.c
insch.c: SCCS/s.insch.c ; sccs get insch.c
insertln.c: SCCS/s.insertln.c ; sccs get insertln.c
llib-lcurses: SCCS/s.llib-lcurses ; sccs get llib-lcurses
longname.c: SCCS/s.longname.c ; sccs get longname.c
move.c: SCCS/s.move.c ; sccs get move.c
mvprintw.c: SCCS/s.mvprintw.c ; sccs get mvprintw.c
mvscanw.c: SCCS/s.mvscanw.c ; sccs get mvscanw.c
mvwin.c: SCCS/s.mvwin.c ; sccs get mvwin.c
newwin.c: SCCS/s.newwin.c ; sccs get newwin.c
overlay.c: SCCS/s.overlay.c ; sccs get overlay.c
overwrite.c: SCCS/s.overwrite.c ; sccs get overwrite.c
printw.c: SCCS/s.printw.c ; sccs get printw.c
refresh.c: SCCS/s.refresh.c ; sccs get refresh.c
scanw.c: SCCS/s.scanw.c ; sccs get scanw.c
scroll.c: SCCS/s.scroll.c ; sccs get scroll.c
standout.c: SCCS/s.standout.c ; sccs get standout.c
touchwin.c: SCCS/s.touchwin.c ; sccs get touchwin.c
tstp.c: SCCS/s.tstp.c ; sccs get tstp.c
unctrl.c: SCCS/s.unctrl.c ; sccs get unctrl.c
unctrl.h: SCCS/s.unctrl.h ; sccs get unctrl.h
