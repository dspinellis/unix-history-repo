OBJS=acos.o asin.o atan.o cbrt.o exp.o log.o sin.o \
      sinh.o sqrt.o tan.o tanh.o

libnm.a : $(OBJS)
	ar r libnm.a $(OBJS)

.s.o :
	as -o $@ $<

.c.o :
	cc -O -c $*.c

install:
	mv libnm.a ${DESTDIR}/usr/lib

clean:
	rm -f *.o libnm.a
