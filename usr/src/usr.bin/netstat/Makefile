# @(#)Makefile	4.3 %G%
OBJS=	host.o inet.o if.o main.o mbuf.o route.o
CFLAGS=-O
DESTDIR=

.c.o:
	${CC} -S ${CFLAGS} $*.c
	sed -f ../asm.sed $*.s | ${AS} -o $*.o
	rm -f $*.s

netstat: ${OBJS}
	${CC} ${OBJS} -o netstat

install: netstat
	install -s netstat ${DESTDIR}/usr/ucb

clean:
	rm -f *.o core a.out errs
