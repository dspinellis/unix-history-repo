FORTUNES=	scene obscene
SOURCE=		fortune.c strfile.h strfile.c rnd.c unstr.c $(FORTUNES)
TFILES=		Troff.mac Troff.sed Do_troff
LIBDIR=		/usr/games/lib
BINDIR=		/usr/games
OWN=		arnold
GRP=		arpa
DEFS=
CFLAGS=		-O $(DEFS)
SFLAGS=		-r
TDEV=		-Pver
TROFF=		ditroff $(TDEV)
DESTDIR=

all: fortune strfile unstr fortunes.dat

fortune: fortune.o rnd.o
	$(CC) $(CFLAGS) -o fortune fortune.o rnd.o

strfile: strfile.o rnd.o
	$(CC) $(CFLAGS) -o strfile strfile.o rnd.o

unstr: unstr.o
	$(CC) $(CFLAGS) -o unstr unstr.o

fortune.o strfile.o unstr.o: strfile.h

fortunes.dat: fortunes strfile
	./strfile $(SFLAGS) fortunes

fortunes: $(FORTUNES)
	cp scene fortunes
	echo "%-" >> fortunes
	cat obscene >> fortunes

lint:
	lint -hxb $(DEFS) fortune.c rnd.c 2>&1 > fortune.lint
	lint -hxb $(DEFS) strfile.c rnd.c 2>&1 > strfile.lint
	lint -hxb $(DEFS) unstr.c 2>&1 > unstr.lint

install: all install.data
	install -s -m 4711 -o daemon fortune $(DESTDIR)$(BINDIR)

install.data: fortunes.dat
	install -m 600 -o daemon fortunes.dat $(DESTDIR)$(LIBDIR)

troff: troff.scene troff.obscene

troff.scene:
	./Do_troff scene $(TROFF)

troff.obscene:
	./Do_troff obscene $(TROFF)

clean: sort.clean
	rm -f fortune fortunes fortunes.dat strfile unstr ? core *.o

sort: sort.scene sort.obscene

sort.scene: strfile unstr
	strfile -oi scene
	mv scene Oscene
	unstr -o scene

sort.obscene: strfile unstr
	strfile -oi obscene
	mv obscene Oobscene
	unstr -o obscene

sort.clean:
	rm -f Oscene Oobscene

depend:
