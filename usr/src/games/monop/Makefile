#
# Nifty foo monopoly maker
#	Program:  Ken Arnold
#	Makefile:  K.S.
#
DESTDIR=
DATFILES=brd.dat mon.dat prop.dat
CFILES=	monop.c cards.c execute.c getinp.c houses.c jail.c misc.c morg.c \
	print.c prop.c rent.c roll.c spec.c strcmp.c trade.c
OBJS=	monop.o cards.o execute.o getinp.o houses.o jail.o misc.o morg.o \
	print.o prop.o rent.o roll.o spec.o strcmp.o trade.o strings.o
HEADERS=monop.h deck.h
BINDIR=	${DESTDIR}/usr/games
LIBDIR=	${DESTDIR}/usr/games/lib
LIB=
CFLAGS=	-O
DAT=	brd.dat monop.dat prop.dat
CC=	cc
XSTR=	xstr
ED=	ed
AS=	as
RM=	rm -f
CTAGS=	ctags

#
# Be cool about compiling strings.
#
.c.o:
	${CC} -E ${CFLAGS} $*.c | ${XSTR} -c -
	${CC} -c ${CFLAGS} x.c
	mv x.o $*.o

it:	monop cards.pck

monop:	${OBJS}
	${CC} -o monop -n ${OBJS} ${LIB}

install: monop cards.pck
	${RM} -f ${BINDIR}/monop ${LIBDIR}/cards.pck
	strip monop
	mv monop ${BINDIR}
	mv cards.pck ${LIBDIR}

new:	clean ctags monop

strings.o: strings
	${XSTR}
	${CC} -S xs.c
	${ED} - < :rofix xs.s
	${AS} -o strings.o xs.s
	${RM} xs.s xs.c

monop.o: ${DATFILES} monop.def
	${CC} -E ${CFLAGS} monop.c | ${XSTR} -c -
	${CC} -c ${CFLAGS} x.c
	mv x.o monop.o

cards.o: deck.h
	${CC} -E ${CFLAGS} cards.c | ${XSTR} -c -
	${CC} -c ${CFLAGS} x.c
	mv x.o cards.o

newone:
	${CC} -o monop ${CFLAGS} ${CFILES} ${LIB}

ctags:
	${CTAGS} ${CFILES} monop.h deck.h monop.def initdeck.c

cards.pck: initdeck cards.inp
	./initdeck

initdeck: initdeck.c deck.h
	${CC} -o initdeck ${CFLAGS} initdeck.c ${LIB}

monop.tar:
	tar crvf monop.tar ${CFILES} ${DATFILES} ${HEADERS} :rofix Makefile cards.inp initdeck.c monop.def monop.ext

clean:
	${RM} ${OBJS} monop core tags x.c x.o monop.tar
	cp /dev/null strings

printall:
	-/usr/bin/eecs/pr monop.h deck.h monop.def monop.ext ${CFILES} ${DATFILES} | /usr/bin/lpr
	-/usr/bin/lpq

lint:
	lint ${CFILES} > lint.out ;
