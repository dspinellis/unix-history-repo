#	Makefile	5.1	85/06/25
#
CFLAGS=-O
TAGSFILE=tags

all: libdbm.a libdbm_p.a

libdbm.a: dbm.c
	${CC} -c ${CFLAGS} dbm.c
	ld -r -o libdbm.a -x dbm.o
	rm -f dbm.o

libdbm_p.a: dbm.c
	${CC} -p -c ${CFLAGS} dbm.c
	ld -r -o libdbm_p.a -X dbm.o
	rm -f dbm.o

install: all
	install -m 644 libdbm.a $(DESTDIR)/usr/lib
	install -m 644 libdbm_p.a $(DESTDIR)/usr/lib
	-cmp -s dbm.h $(DESTDIR)/usr/include/dbm.h || \
		install -c -m 644 dbm.h $(DESTDIR)/usr/include

tags:
	ctags -a -f ${TAGSFILE} `pwd`/dbm.c

clean:
	rm -f *.o libdbm.a libdbm_p.a errs a.out core
