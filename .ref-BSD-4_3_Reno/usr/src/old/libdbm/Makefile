#	Makefile	4.6	85/06/25
#
CFLAGS=-O
TAGSFILE=tags

libdbm.a: dbm.c
	${CC} -c ${CFLAGS} dbm.c
	mv dbm.o libdbm.a

install:
	install -m 644 libdbm.a $(DESTDIR)/usr/lib/libodbm.a

tags:
	ctags -a -f ${TAGSFILE} `pwd`/dbm.c

clean:
	rm -f libdbm.a errs a.out core
