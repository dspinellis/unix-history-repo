#	Makefile	4.3	83/10/17
#
CFLAGS=-O

libdbm.a: dbm.c
libdbm.a: dbm.h
	${CC} -c ${CFLAGS} dbm.c
	mv dbm.o libdbm.a

install:
	install -m 644 libdbm.a $(DESTDIR)/usr/lib
	install -c dbm.h $(DESTDIR)/usr/include

clean:
	rm -f libdbm.a errs a.out core
