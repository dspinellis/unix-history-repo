#	@(#)Makefile	4.2	(Berkeley)	83/08/14
#
DESTDIR=
FORTUNES=	scene obscene
SOURCE=		fortune.c strfile.h strfile.c unstr.c ${FORTUNES}
LIBDIR=		${DESTDIR}/usr/games/lib
BINDIR=		${DESTDIR}/usr/games
OWN=		daemon
CFLAGS=		-O

.DEFAULT:
	sccs get $@

all: fortune strfile unstr fortunes.dat

fortune: strfile.h fortune.c
	${CC} ${CFLAGS} -DFORTFILE='"${LIBDIR}/fortunes.dat"' -o fortune fortune.c

strfile: strfile.h strfile.c
	${CC} ${CFLAGS} -o strfile strfile.c

unstr: strfile.h unstr.c
	${CC} ${CFLAGS} -o unstr unstr.c

fortunes.dat: fortunes strfile
	./strfile fortunes

fortunes: $(FORTUNES)
	cat scene > fortunes
	echo "%-" >> fortunes
	cat obscene >> fortunes
	echo "%%" >> fortunes

install: all
	install -m 600 -o ${OWN} fortunes.dat ${LIBDIR}/
	install -m 4711 -o ${OWN} fortune ${BINDIR}/

clean:
	rm -f fortune fortunes fortunes.dat fortunes.tar strfile unstr
