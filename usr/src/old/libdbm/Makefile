#	Makefile	4.4	83/12/02
#
CFLAGS=-O

libdbm.a: dbm.c
	${CC} -c ${CFLAGS} dbm.c
	mv dbm.o libdbm.a

install:
	install -m 644 libdbm.a $(DESTDIR)/usr/lib

clean:
	rm -f libdbm.a errs a.out core
