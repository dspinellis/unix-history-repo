#	@(#)Makefile	5.6 (Berkeley) %G%

PROG=	fish
MAN6=	fish.0
HIDEGAME=hidegame

beforeinstall: 
	install -c -o bin -g bin -m 444 ${.CURDIR}/fish.instr ${DESTDIR}/usr/share/games

.include <bsd.prog.mk>
