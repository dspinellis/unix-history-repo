#
#	@(#)Makefile	5.2	(Berkeley)	86/07/26
#
DESTDIR=

libg.a:	dbxxx.c
	${CC} -E dbxxx.c | ${AS} -o libg.a

install: libg.a
	install -c libg.a ${DESTDIR}/usr/lib

clean:
	rm -f libg.a
