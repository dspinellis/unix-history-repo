#	@(#)Makefile	4.1	(Berkeley)	10/29/82
#
DESTDIR=
CFLAGS=	-O

all:

install:
	install -c ching.sh ${DESTDIR}/usr/games/ching
	-mkdir ${DESTDIR}/usr/games/lib/ching.d
	install -c cno ${DESTDIR}/usr/games/lib/ching.d/cno
	install -c phx ${DESTDIR}/usr/games/lib/ching.d/phx
	install -c macros ${DESTDIR}/usr/games/lib/ching.d/macros
	install -c hexagrams ${DESTDIR}/usr/games/lib/ching.d/hexagrams

clean:
	rm -f a.out core *.s *.o
