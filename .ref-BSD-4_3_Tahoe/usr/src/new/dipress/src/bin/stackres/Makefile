#
#
# Copyright (c) 1984, 1985, 1986 Xerox Corp.
#
#	Makefile for "stackres" module
#
CFLAGS=		-I${DESTINCLUDE} -O
LFLAGS=		-lm
DESTBIN=	../../../bin
DESTINCLUDE=	../../../include

stackres: stackres.o parse.o execute.o stack.o error.o
	cc -o stackres stackres.o parse.o execute.o stack.o error.o ${LFLAGS}

stackres.o: stackres.c stack.h /usr/include/stdio.h
	cc -c ${CFLAGS} stackres.c

parse.o: parse.c ${DESTINCLUDE}/iptokens.h /usr/include/stdio.h
	cc -c ${CFLAGS} parse.c

execute.o: execute.c stack.h ${DESTINCLUDE}/iptokens.h /usr/include/stdio.h /usr/include/math.h
	cc -c ${CFLAGS} execute.c

stack.o: stack.c stack.h
	cc -c ${CFLAGS} stack.c

lint:
	lint -abu stackres.c error.c

clean:
	rm -f stackres *.o

install: ${DESTBIN}/stackres

${DESTBIN}/stackres: stackres
	install -c -s stackres ${DESTBIN}
