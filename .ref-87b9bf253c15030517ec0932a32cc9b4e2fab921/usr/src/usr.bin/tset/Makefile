DESTDIR=
CFLAGS=-O
tset:	tset.c
	${CC} ${CFLAGS} tset.c -o tset -ltermlib
install:
	install -s tset ${DESTDIR}/usr/ucb
clean:
	rm -f *.o tset
