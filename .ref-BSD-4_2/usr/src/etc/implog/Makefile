# @(#)Makefile	4.5 6/12/83
ALL=	implog implogd
DESTDIR=
CFLAGS=-O

all:	${ALL}

implog:	implog.o
	${CC} implog.o -o implog

implogd: implogd.o
	${CC} implogd.o -o implogd

clean:
	rm -f ${ALL} *.o *.s errs core a.out t.?

install: ${ALL}
	install implog ${DESTDIR}/etc
	install implogd ${DESTDIR}/etc
