CFLAGS=-O

libdbm.a: dbm.o
	mv dbm.o libdbm.a

dbm.o : dbm.h

install:
	install -c libdbm.a $(DESTDIR)/usr/lib
	install -c dbm.h $(DESTDIR)/usr/include

clean:
	rm libdbm.a
