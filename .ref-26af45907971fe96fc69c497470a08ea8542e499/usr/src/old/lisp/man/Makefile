#	@(#)Makefile	5.2 (Berkeley) %G%

MAN=	lisp.0 liszt.0 lxref.0

all clean depend lint tags:

cleandir:
	rm -f ${MAN}

install: ${MAN}
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat1
