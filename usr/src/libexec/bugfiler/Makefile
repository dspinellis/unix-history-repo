#	Makefile	4.3.1.1	85/11/04
#
# Bug report processor and associated programs
#

CFLAGS = -O -DUNIXCOMP
LIBDIR=	/usr/lib
DESTDIR=

all: bugfiler unixtomh

bugfiler: bugfiler.c
	${CC} ${CFLAGS} -o bugfiler bugfiler.c

unixtomh: unixtomh.c
	${CC} ${CFLAGS} -o unixtomh unixtomh.c

install: bugfiler sendbug.sh bugformat
	install -s bugfiler ${DESTDIR}${LIBDIR}/bugfiler
	install -c sendbug.sh ${DESTDIR}/usr/ucb/sendbug
	install -c -m 644 bugformat ${DESTDIR}/usr/ucb/bugformat

clean:
	rm -f *.o bugfiler
