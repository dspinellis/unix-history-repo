# @(#)Makefile	4.3 1/13/83
ALL=	rsh rshd 
DESTDIR=
CFLAGS=-O

all:	${ALL}

rsh:	rsh.o
	${CC} rsh.o -o rsh -ljobs

rshd:	rshd.o
	${CC} rshd.o -o rshd -ljobs

clean:
	rm -f ${ALL} *.o *.s

install: ${ALL}
	install -s -m 4755 rsh ${DESTDIR}/usr/ucb
	install -s rshd ${DESTDIR}/etc
