SOURCE=	fortune.c strfile.h strfile.c unstr.c scene obscene
LIBDIR=	/usr/games/lib
BINDIR=	/usr/games
OWN=	arnold
GRP=	arpa
CFLAGS=	-O -s
TARF=	fortunes.tar

all: fortune strfile unstr fortunes.dat

fortune: strfile.h fortune.c
	${CC} ${CFLAGS} -DFORTFILE='"${LIBDIR}/fortunes.dat"' -o fortune fortune.c
	
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
	mv fortunes.dat ${LIBDIR}/fortunes.dat
	chown ${OWN} ${LIBDIR}/fortunes.dat
	chgrp ${GRP} ${LIBDIR}/fortunes.dat
	chmod 600 ${LIBDIR}/fortunes.dat
	mv fortune ${BINDIR}
	chown ${OWN} ${BINDIR}/fortune
	chgrp ${GRP} ${BINDIR}/fortune
	chmod 4711 ${BINDIR}/fortune

tar:
	tar crvf ${TARF} Makefile ${SOURCE} fortune.6

clean:
	rm -f fortune fortunes fortunes.dat fortunes.tar strfile unstr
