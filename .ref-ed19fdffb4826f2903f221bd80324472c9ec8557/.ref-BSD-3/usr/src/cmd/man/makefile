all:
	cc -O man.c -o man

install:
	cp man.sh ${DESTDIR}/usr/bin/man
	chmod 755 ${DESTDIR}/usr/bin/man
	cp man ${DESTDIR}/usr/ucb/man

clean:
	rm -f man
