# @(#)Makefile	4.2 %G%
ALL=	implog implogd
DESTDIR=
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
	install implog ${DESTDIR}/usr/ucb
	install implogd ${DESTDIR}/etc
