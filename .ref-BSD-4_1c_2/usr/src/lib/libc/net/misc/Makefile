# @(#)Makefile	4.1 12/15/82
OBJS=	bcmp.o bcopy.o bzero.o ffs.o \
	htons.o ntohs.o htonl.o ntohl.o \
	insque.o remque.o
CFLAGS=	

.c.o:
	/lib/cpp -E -DPROF $*.c | ${AS} -o $*.o
	-ld -x -r $*.o
	mv a.out profiled/$*.o
	/lib/cpp -E $*.c | ${AS} -o $*.o
	-ld -x -r $*.o
	mv a.out $*.o

all:	${OBJS}

clean:
	rm -f ${OBJS} profiled/*.o errs a.out core
