#	@(#)Makefile	5.12 (Berkeley) 5/30/93

Missing: adventure boggle ching dungeon hunt phantasia quiz warp 

PROG=	larn
MAN6=	larn.0
CFLAGS+=-DBSD -DVER=12 -DSUBVER=0 -DNONAP -DUIDSCORE -fwritable-strings
SRCS=	main.c object.c create.c tok.c display.c global.c data.c io.c \
	monster.c store.c diag.c help.c config.c nap.c bill.c scores.c \
	signal.c moreobj.c movem.c regen.c fortune.c savelev.c
DPADD=	${LIBTERM} ${LIBCOMPAT}
LDADD=	-ltermcap -lcompat
HIDEGAME=hidegame

beforeinstall:
	(cd ${.CURDIR}/datfiles; install -c -o ${BINOWN} -g ${BINGRP} -m 444 \
	    larnmaze larnopts larn.help \
	    ${DESTDIR}/usr/share/games/larn)

.include <bsd.prog.mk>
