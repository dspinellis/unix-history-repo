DESTDIR=
CFLAGS=-O -DUCB_NTTY
tset:	tset.c
	${CC} ${CFLAGS} tset.c -o tset -ltermlib
install:
	install -s tset ${DESTDIR}/usr/ucb
clean:
	rm -f *.o tset
