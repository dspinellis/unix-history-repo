#	Makefile	4.3	83/03/10

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
	install -c -m 755 ccat.sh ${DESTDIR}/usr/ucb/ccat

clean:
	rm -f compact.o uncompact.o tree.o
