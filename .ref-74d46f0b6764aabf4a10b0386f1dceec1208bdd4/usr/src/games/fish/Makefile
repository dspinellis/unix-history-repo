#	@(#)Makefile	5.5 (Berkeley) %G%

PROG=	fish
MAN6=	fish.0
HIDEGAME=hidegame

beforeinstall: 
	install -c -o bin -g bin -m 444 ${.CURDIR}/fish.instr /usr/share/games

.include <bsd.prog.mk>
