CFLAGS=-O
FILES= asin.o atan.o hypot.o jn.o j0.o j1.o pow.o gamma.o \
	log.o sin.o sqrt.o tan.o tanh.o sinh.o erf.o exp.o floor.o

libm.a : $(FILES)
	ar rc libm.a $(FILES)

install: libm.a
	cp libm.a ${DESTDIR}/usr/lib
	ranlib ${DESTDIR}/usr/lib/libm.a

clean:
	rm -f *.o libm.a
