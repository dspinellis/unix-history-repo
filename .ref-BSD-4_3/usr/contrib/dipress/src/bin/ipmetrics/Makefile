#
# (C) Copyright 1985, 1986 Xerox Corp.
#
#	Makefile for "ipmetrics" module
#
CFLAGS =	-g -DDESTLIB=\"${DESTLIB}\" -I${DESTINCLUDE}
LINTFLAGS = 	-DDESTLIB=\"${DESTLIB}\" -I${DESTINCLUDE}
LFLAGS =	-lm
DESTBIN =	../../../bin
DESTINCLUDE =	../../../include
DESTLIB =	../../../lib

ipmetrics: ipmetrics.o troff.o tex.o parse.o execute.o stack.o error.o token.o getopt.o
	cc -g -o ipmetrics ipmetrics.o troff.o tex.o parse.o execute.o stack.o error.o token.o getopt.o ${LFLAGS}

ipmetrics.o: ipmetrics.c ipmetrics.h config.h stack.h token.h /usr/include/stdio.h
	cc -c ${CFLAGS} ipmetrics.c

troff.o: ipmetrics.h config.h ipmetrics.h troff.h

parse.o: parse.c ${DESTINCLUDE}/iptokens.h /usr/include/stdio.h
	cc -c ${CFLAGS} parse.c

execute.o: execute.c stack.h ${DESTINCLUDE}/iptokens.h /usr/include/stdio.h /usr/include/math.h
	cc -c ${CFLAGS} execute.c

stack.o: stack.c stack.h
	cc -c ${CFLAGS} stack.c

clean:
	rm -f ipmetrics *.o

install: ipmetrics
	install -c -s ipmetrics ${DESTBIN}

lint:
	lint -abc -I$(DESTINCLUDE) ipmetrics.c troff.c parse.c execute.c stack.c error.c token.c $(DESTLIB)/llib-lip.ln -lc
