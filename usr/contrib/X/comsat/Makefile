#
# $Header: Makefile,v 10.3 86/02/01 15:18:43 tony Rel $
#

DESTDIR=
CFLAGS=	-O


all:	comsat

install:
	install -c -s ${DESTDIR}/etc

clean:
	rm -f comsat *.o *~

comsat:
	cc ${CFLAGS} -o $@ $@.c ../Xlib/libX.a
