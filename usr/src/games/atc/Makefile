PROG=	atc
CFLAGS+=-DBSD -I${.CURDIR}
SRCLIBS=/usr/lib/libl.a ${LIBM} ${LIBTERM} ${LIBCURSES} ${LIBCOMPAT}
LDFILES=-ll -lm -lcurses -ltermcap -lcompat
SRCS=	extern.c grammar.c graphics.c input.c lex.c list.c log.c \
	main.c tunable.c update.c
HIDEGAME=hidegame
MAN6=	atc.0
CLEANFILES=grammar.c y.tab.h lex.c
GAMES=	ATC_scores Game_List Killer crossover default easy game_2

beforeinstall: 
	(cd ${.CURDIR}/games; install -c -o ${BINOWN} -g ${BINGRP} -m 400 \
	    ${GAMES} ${DESTDIR}/usr/share/games/atc)

.include <bsd.prog.mk>
