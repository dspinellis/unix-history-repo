#
#	@(#)Makefile	5.1	(Berkeley)	85/07/12
#
libg.a:	dbxxx.s
	as dbxxx.s -o libg.a

install: libg.a
	install -c libg.a ${DESTDIR}/usr/lib

clean:
	rm -f libg.a
