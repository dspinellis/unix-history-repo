#	@(#)Makefile	5.9 (Berkeley) 5/11/90

PROG=	phantasia
CFLAGS+=-DBSD42
SRCS=	main.c fight.c io.c interplayer.c gamesupport.c misc.c phantglobs.c
MAN6=	phantasia.0
DPADD=	${LIBM} ${LIBCURSES} ${LIBTERM} ${LIBCOMPAT}
LDADD=	-lm -lcurses -ltermlib -lcompat
HIDEGAME=hidegame

phantasia.0: phantasia.6
	tbl -TX ${.CURDIR}/phantasia.6 | nroff -h -man > ${.TARGET}

all: setup

setup: phantglobs.o setup.o monsters.asc ${LIBC} ${LIBM} 
	${CC} phantglobs.o setup.o -o ${.TARGET} -lm

beforeinstall:
	(cd ${.CURDIR}; obj/setup)
	chown games.bin /var/games/phantasia/*

# Make Phantasia map.  Change the map commands reflect your installation.
# PLOTDEVICE is used for plotting the map.  Change as appropriate.

map: map.c
	${CC} -O ${.CURDIR}/map.c -lplot -o ${.TARGET}
	./map | plot > /dev/tty

.include <bsd.prog.mk>
