#
# Copyright (c) 1984, 1985, 1986 Xerox Corp.
#
#	Makefile for "restotext" module
#
DESTBIN=	../../../bin
DESTINCLUDE=	../../../include
DESTLIB=	../../../lib

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

lint:
	lint -abu restotext.c enumerate.c

clean:
	rm -f restotext *.o

install: ${DESTBIN}/restotext

${DESTBIN}/restotext: restotext
	install -c -s restotext ${DESTBIN}
