all:
	cc -O -d2 ed.c -o ed
install:
	mv ed ${DESTDIR}/bin/ed
	rm -f ${DESTDIR}/bin/e
	ln ${DESTDIR}/bin/ed ${DESTDIR}/bin/e
clean:
	rm -f ed
