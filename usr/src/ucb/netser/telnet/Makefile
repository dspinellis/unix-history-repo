# @(#)Makefile	4.5 2/23/83
ALL=	telnet telnetd
DESTDIR=
CFLAGS=-O

all:	${ALL}

telnet:	telnet.o
	${CC} telnet.o -o telnet -ljobs

telnetd: telnetd.o
	${CC} telnetd.o -o telnetd -ljobs

clean:
	rm -f ${ALL} *.o *.s errs core a.out t.?

install: ${ALL}
	install -s telnet ${DESTDIR}/usr/ucb
	install -s telnetd ${DESTDIR}/etc
