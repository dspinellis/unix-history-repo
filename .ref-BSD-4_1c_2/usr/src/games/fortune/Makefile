#	@(#)Makefile	4.1 (Berkeley) 2/14/83
#
SOURCE=	fortune.c strfile.h strfile.c unstr.c scene obscene
LIBDIR=	/usr/games/lib
BINDIR=	/usr/games
OWN=	daemon
GRP=	daemon
CFLAGS=	-O
TARF=	fortunes.tar
DESTDIR=

.DEFAULT:
	sccs get $@

all: fortune strfile unstr fortunes.dat

fortune: strfile.h fortune.c
	${CC} ${CFLAGS} -DFORTFILE='"${LIBDIR}/fortunes.dat"' \
		-o fortune fortune.c
	
strfile: strfile.h strfile.c
	${CC} ${CFLAGS} -o strfile strfile.c

unstr: strfile.h unstr.c
	${CC} ${CFLAGS} -o unstr unstr.c

fortunes.dat: fortunes strfile
	./strfile fortunes

fortunes: scene obscene
	cat scene > fortunes
	echo "%-" >> fortunes
	cat obscene >> fortunes
	echo "%%" >> fortunes

install: all
	install -m 600 -o ${OWN} fortunes.dat \
		${DESTDIR}/${LIBDIR}/fortunes.dat
	chgrp ${GRP} ${DESTDIR}/${LIBDIR}/fortunes.dat
	install -s -m 4711 -o ${OWN} fortune \
		${DESTDIR}/${BINDIR}/fortune
	chgrp ${GRP} ${DESTDIR}/${LIBDIR}/fortunes.dat

tar:
	tar crvf ${TARF} Makefile ${SOURCE} fortune.6

clean:
	rm -f fortune fortunes fortunes.dat fortunes.tar strfile unstr
