#	@(#)Makefile	5.2 (Berkeley) 6/25/90

FFLAGS=	-g -Ddebug -DNOCC -q # -g -Ddebug
OBJS=	actors.o ballop.o clockr.o demons.o dgame.o dinit.o dmain.o \
	dso1.o dso2.o dso3.o dso4.o dso5.o dso6.o dso7.o dsub.o \
	dverb1.o dverb2.o gdt.o lightp.o nobjs.o np.o np1.o np2.o \
	np3.o nrooms.o objcts.o rooms.o rtim.o sobjs.o sverbs.o \
	verbs.o villns.o
MAN=	dungeon.0

all: dungeon

dungeon: ${OBJS}
	f77 -g -o $@ ${OBJS} rand.o ecvt.o
clean:
	rm -f ${OBJS} core dungeon

cleandir: clean
	rm -f ${MAN} tags .depend

depend lint tags:

install: ${MAN}
	-[ -d ${DESTDIR}/usr/games/lib/dunlib ] || mkdir ${DESTDIR}/usr/games/lib/dunlib
	chmod 700 ${DESTDIR}/usr/games/lib/dunlib
	chown games.bin ${DESTDIR}/usr/games/lib/dunlib
	install -s -o games -g bin -m 4700 dungeon ${DESTDIR}/usr/games/hide
	(cd ${DESTDIR}/usr/games; rm -f dungeon; ln -s dm dungeon; chown games.bin dungeon)
	install -c -o bin -g bin -m 444 dindx.dat dtext.dat ${DESTDIR}/usr/games/lib/dunlib
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat6
	rm -f ${DESTDIR}/usr/man/cat6/zork.0
	ln ${DESTDIR}/usr/man/cat6/dungeon.0 ${DESTDIR}/usr/man/cat6/zork.0
