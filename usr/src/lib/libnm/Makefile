CFLAGS=-O
OBJS=acos.o asin.o atan.o cbrt.o pow.o exp.o log.o sin.o \
      sinh.o sqrt.o tan.o tanh.o

libnm.a : $(OBJS)
	ar r libnm.a $(OBJS)

install:
	mv libnm.a ${DESTDIR}/usr/lib/libnm.a
	ranlib ${DESTDIR}/usr/lib/libnm.a

clean:
	rm -f *.o libnm.a
