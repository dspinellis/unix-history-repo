# @(#)Makefile	4.4 12/18/82
OBJS=	host.o inet.o if.o main.o mbuf.o route.o
CFLAGS=-O
DESTDIR=

netstat: ${OBJS}
	${CC} ${OBJS} -o netstat

install: netstat
	install -s netstat ${DESTDIR}/usr/ucb

clean:
	rm -f *.o core a.out errs
