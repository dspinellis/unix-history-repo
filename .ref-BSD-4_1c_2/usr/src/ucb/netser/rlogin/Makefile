# @(#)Makefile	4.3 2/23/83
ALL=	rlogin rlogind
DESTDIR=
CFLAGS=-O

all:	${ALL}

rlogin:	rlogin.o
	${CC} rlogin.o -o rlogin

rlogind: rlogind.o
	${CC} rlogind.o -o rlogind -ljobs

clean:
	rm -f ${ALL} *.o *.s

install: ${ALL}
	install -s -m 4755 rlogin ${DESTDIR}/usr/ucb
	install -s rlogind ${DESTDIR}/etc
