#
#	@(#)Makefile	1.3	(Berkeley)	83/03/30
#
DESTDIR=
SRCS=	dc.c dc.h
CFLAGS=	-O

all: dc

dc:	dc.c dc.h
	${CC} ${CFLAGS} dc.c -o dc

install:
	install -s dc ${DESTDIR}/usr/bin/dc

clean:
	rm -f *.o
