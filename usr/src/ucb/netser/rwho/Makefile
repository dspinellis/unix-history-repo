# @(#)Makefile	4.3 2/23/83
ALL=	ruptime rwho rwhod 
DESTDIR=
CFLAGS=-O

all:	${ALL}

ruptime.o rwho.o rwhod.o: rwhod.h

ruptime: ruptime.o
	${CC} ruptime.o -o ruptime -ltermlib

rwho:	rwho.o
	${CC} rwho.o -o rwho

rwhod:	rwhod.o
	${CC} rwhod.o -o rwhod -ljobs

clean:
	rm -f ${ALL} *.o *.s

install: ${ALL}
	install -s rwho ${DESTDIR}/usr/ucb
	install -s rwhod ${DESTDIR}/etc
	install -s ruptime ${DESTDIR}/usr/ucb
