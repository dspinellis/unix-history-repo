#
#	@(#)Makefile	5.1	(Berkeley)	87/05/31
#
DESTDIR=
SRCS=	dc.c dc.h
CFLAGS=	-O

all: dc

dc:	dc.c dc.h
	${CC} ${CFLAGS} dc.c -o dc

install: dc
	install -s dc ${DESTDIR}/usr/bin/dc

clean:
	rm -f *.o dc a.out errs core

depend:
