#
# Makefile for dlpt
#

DESTDIR=
AS=/bin/as
CC=/bin/cc
LCC=/bin/cc
LD=/bin/ld
INCLDIR=${DESTDIR}/usr/include
LIBDIR=${DESTDIR}/lib
ULIBDIR=${DESTDIR}/usr/lib

DEST=${DESTDIR}/lib/pt
PT2=PT2.131.XXX

dlpt: dlpt.c
	${CC} -DV7 -DNEWSIG -O -o dlpt dlpt.c

recnv: recnv.c
	${LCC} -O -o recnv recnv.c

DEFAULT.LD: recnv DEFAULT
	./recnv DEFAULT 1
	mv hexout hex1
	./recnv hex1 2
	mv hexout hex2
	./recnv hex2 3
	mv hexout DEFAULT.LD

PT2DEF.LD:
	cp $(PT2) PT2DEF.LD

install: dlpt DEFAULT.LD PT2DEF.LD
	install -s dlpt $(DEST)
	install DEFAULT.LD $(DEST)
	install PT2DEF.LD $(DEST)

clean:
	rm -f dlpt recnv hexout hex1 hex2 *.LD  *.o
	
cleansrc:
	rm -f *.c *.n Makefile term
