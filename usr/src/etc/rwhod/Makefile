#	Makefile	4.1	83/07/01
#
CFLAGS=-O
DESTDIR=

rwhod:	rwhod.o
	${CC} -o rwhod rwhod.o

install: rwhod
	install -s rwhod ${DESTDIR}/etc/rwhod
	-mkdir ${DESTDIR}/usr/spool/rwho

clean:
	rm -f rwhod rwhod.o a.out core errs
