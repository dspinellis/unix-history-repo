#	@(#)Makefile	5.2 (Berkeley) 7/1/91

MAN=	lisp.0 liszt.0 lxref.0

all clean depend lint tags:

cleandir:
	rm -f ${MAN}

install: ${MAN}
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat1
