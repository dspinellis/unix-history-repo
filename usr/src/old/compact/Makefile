#	Makefile	4.2	83/02/23

CFLAGS=-O
DESTDIR=

all: compact uncompact

compact: compact.o tree.o
	cc compact.o tree.o -o compact

uncompact: uncompact.o tree.o
	cc uncompact.o tree.o -o uncompact

compact.o uncompact.o tree.o: compact.h

install:
	install -s compact ${DESTDIR}/usr/ucb
	install -s uncompact ${DESTDIR}/usr/ucb
	cp ccat.sh ${DESTDIR}/usr/ucb/ccat
	chmod 755 ${DESTDIR}/usr/ucb/ccat

clean:
	rm -f compact.o uncompact.o tree.o
