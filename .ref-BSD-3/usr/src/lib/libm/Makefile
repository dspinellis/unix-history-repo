CFLAGS=-O
FILES=asin.o atan.o exp.o erf.o floor.o hypot.o j0.o j1.o \
      jn.o log.o pow.o fabs.o sin.o sinh.o sqrt.o tan.o tanh.o gamma.o

libm.a : $(FILES)
	ar rc libm.a asin.o atan.o hypot.o jn.o j0.o j1.o pow.o gamma.o \
	log.o sin.o sqrt.o tan.o tanh.o sinh.o erf.o exp.o floor.o fabs.o

install: libm.a
	install libm.a ${DESTDIR}/usr/lib

clean:
	rm -f *.o libm.a
