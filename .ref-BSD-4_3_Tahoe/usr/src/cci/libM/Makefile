#
#	Makefile for the new floating point library (5/22/85)
#

CC=/bin/cc
CFLAGS=-O -c
DESTDIR=
INSTALL=install


libM.a: 	atand.o expd.o logd.o sind.o sqrtd.o
		ar rv libM.a atand.o expd.o logd.o sind.o sqrtd.o
		ranlib libM.a

atand.o:	atand.c atand.h
		${CC} ${CFLAGS} atand.c

atand.h:	atand.bc atand.awk
		rm -f atand.out
		bc -l atand.bc > atand.out
		rm -f atand.h
		sh ./atand.awk atand.out > atand.h
		rm -f atand.out

expd.o: 	expd.c expd.tab.c
		${CC} ${CFLAGS} expd.c
		${CC} ${CFLAGS} expd.tab.c
		ld -r expd.o expd.tab.o
		mv a.out expd.o
		rm expd.tab.o

expd.tab.c:	expd.bc expd.awk
		rm -f expd.out
		bc -l expd.bc > expd.out
		rm -f expd.tab.c
		sh ./expd.awk expd.out > expd.tab.c
		rm -f expd.out

logd.o: 	logd.c logd.h
		${CC} ${CFLAGS} logd.c

logd.h: 	logd.bc logd.awk
		rm -f logd.out
		bc -l logd.bc > logd.out
		rm -f logd.h
		sh ./logd.awk logd.out > logd.h
		rm -f logd.out

sind.o: 	sind.c
		${CC} ${CFLAGS} sind.c

sqrtd.o:	sqrtd.c
		${CC} ${CFLAGS} sqrtd.c

install:	libM.a
		${INSTALL} libM.a ${DESTDIR}/usr/lib

clean:
		rm -f *.o libM.a
