#
# Copyright (c) 1984, 1985 Xerox Corp.
#
#	Makefile for "restotext" module
#
DESTDIR=	../../../bin
DESTINCLUDE=	../../../include

CFLAGS=		-I${DESTINCLUDE} -O 
LFLAGS=		-lbeacon

restotext: restotext.o parse.o enumerate.o
	cc -o restotext restotext.o parse.o enumerate.o

restotext.o: restotext.c /usr/include/stdio.h
	cc -c -O restotext.c

parse.o: parse.c ${DESTINCLUDE}/iptokens.h /usr/include/stdio.h
	cc -c ${CFLAGS} parse.c

enumerate.o: enumerate.c /usr/include/stdio.h
	cc -c -O enumerate.c

clean:
	rm -f restotext *.o

install: restotext
	install -c -s restotext ${DESTDIR}
