#	Makefile	4.2	83/08/20
#
CFLAGS=-O

libdbm.a: dbm.c
libdbm.a: dbm.h
	${CC} -o libdbm.a ${CFLAGS} dbm.c

install:
	install -m 644 libdbm.a $(DESTDIR)/usr/lib
	install -c dbm.h $(DESTDIR)/usr/include

clean:
	rm -f libdbm.a errs a.out core
