#	Makefile	4.2	83/07/27
#
DESTDIR=
LFLAGS=	-v
CFLAGS=	-O
ALL=	hyroute hylog

all:	${ALL}

hyroute: hyr_main.o hyr_lex.o
	${CC} ${CFLAGS} -o hyroute hyr_main.o hyr_lex.o -ll

hylog:	hylog.c
	${CC} -o hylog ${CFLAGS} hylog.c

install: all
	install -s hyroute ${DESTDIR}/usr/new/hyroute
	install -s hylog ${DESTDIR}/usr/new/hylog

hyr_main.o:	hyr_main.c hyr_sym.h
hyr_lex.o:	hyr_lex.l hyr_sym.h

clean:
	-rm -f *.o hyroute hyr_lex.c hylog
