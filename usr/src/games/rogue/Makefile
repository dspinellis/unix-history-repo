#	@(#)Makefile	4.4	(Berkeley)	83/08/14
#
DESTDIR=
CFLAGS=	-O -DSCOREFILE='"/usr/games/lib/rogue_roll"' -DNAMELIST='"/dev/vmunix"'\
	-DLOADAV -DMAXLOAD=50

rogue: mach_dep.o distmod.obj
	${CC} -o rogue distmod.obj mach_dep.o -lcurses -ltermlib

install: rogue
	install -o daemon -s -m 4711 rogue ${DESTDIR}/usr/games/rogue
	install -c -o daemon -m 644 /dev/null \
		${DESTDIR}/usr/games/lib/rogue_roll

clean:
	rm -f rogue mach_dep.o
