DESTDIR=
CFLAGS=-O

libmp.a: pow.o gcd.o msqrt.o mdiv.o mout.o mult.o madd.o util.o
	ar cr libmp.a *.o

install: libmp.a
	install -c libmp.a ${DESTDIR}/usr/lib

clean:
	rm -f *.o libmp.a
