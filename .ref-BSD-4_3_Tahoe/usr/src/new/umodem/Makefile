#	Makefile	4.4	85/09/07
#
DESTDIR=
CFLAGS=	-O
STD=	umodem

all:	${STD}

umodem:
	cc ${CFLAGS} -DVER7 -o umodem umodem.c

install: all
	install -s umodem ${DESTDIR}/usr/new/umodem
	install -c umodem.n ${DESTDIR}/usr/man/mann/umodem.n

clean:
	rm -f *.o ${STD}
