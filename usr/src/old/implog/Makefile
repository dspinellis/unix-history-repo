# @(#)Makefile	4.1 %G%
ALL=	implog implogd
CFLAGS=-O

all:	${ALL}

.c.o:
	${CC} -S ${CFLAGS} $*.c
	sed -f ../asm.sed $*.s | as -o $*.o
	rm -f $*.s

implog:	implog.o
	${CC} implog.o -o implog -ljobs

implogd: implogd.o
	${CC} implogd.o -o implogd

clean:
	rm -f ${ALL} *.o *.s errs core a.out t.?

install: ${ALL}
	install implog /usr/ucb
	install implogd /etc
