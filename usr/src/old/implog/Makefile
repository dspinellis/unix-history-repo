# @(#)Makefile	4.4 %G%
ALL=	implog implogd
DESTDIR=
CFLAGS=-O

all:	${ALL}

implog:	implog.o
	${CC} implog.o -o implog -ljobs

implogd: implogd.o
	${CC} implogd.o -o implogd

clean:
	rm -f ${ALL} *.o *.s errs core a.out t.?

install: ${ALL}
	install implog ${DESTDIR}/etc
	install implogd ${DESTDIR}/etc
