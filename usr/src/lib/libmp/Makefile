DESTDIR=
CFLAGS=-O
OBJS= pow.o gcd.o msqrt.o mdiv.o mout.o mult.o madd.o util.o

libmp.a: $(OBJS)
	ar cr libmp.a $(OBJS)

install: libmp.a
	install -c libmp.a ${DESTDIR}/usr/lib

clean:
	rm -f *.o libmp.a
