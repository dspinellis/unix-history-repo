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

ipmetrics: ipmetrics.o conf.o troff.o otroff.o tex.o toc.o generic.o parse.o execute.o stack.o error.o token.o $(DESTLIB)/libgetopt.a
	cc -g -o ipmetrics ipmetrics.o conf.o troff.o otroff.o tex.o toc.o generic.o parse.o execute.o stack.o error.o token.o $(DESTLIB)/libgetopt.a ${LFLAGS}

ipmetrics.o: ipmetrics.c ipmetrics.h conf.h config.h stack.h token.h /usr/include/stdio.h
	cc -c ${CFLAGS} ipmetrics.c

conf.o: conf.h

troff.o: config.h ipmetrics.h troff.h

otroff.o: config.h ipmetrics.h otroff.h

tex.o: stack.h token.h config.h ipmetrics.h tex.h

toc.o: ipmetrics.h config.h

generic.o: stack.h token.h config.h ipmetrics.h generic.h

parse.o: parse.c ${DESTINCLUDE}/iptokens.h /usr/include/stdio.h
	cc -c ${CFLAGS} parse.c

execute.o: execute.c stack.h ${DESTINCLUDE}/iptokens.h /usr/include/stdio.h /usr/include/math.h
	cc -c ${CFLAGS} execute.c

stack.o: stack.c stack.h
	cc -c ${CFLAGS} stack.c

clean:
	rm -f ipmetrics *.o

install: ${DESTBIN}/ipmetrics

${DESTBIN}/ipmetrics: ipmetrics
	install -c -s ipmetrics ${DESTBIN}

# not for right now....
# lint:
#	lint -abc -I$(DESTINCLUDE) ipmetrics.c troff.c otroff.c parse.c execute.c stack.c error.c token.c $(DESTLIB)/llib-lip.ln -lc

lint:
	lint -abu -I$(DESTINCLUDE) ipmetrics.c troff.c otroff.c tex.c generic.c token.c error.c $(DESTLIB)/llib-lip.ln -lc
