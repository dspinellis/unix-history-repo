#	Makefile	4.2	83/08/23
#
# Bug report processor and associated programs
#

CFLAGS = -O -DUNIXCOMP
LIBDIR=	/usr/lib
DESTDIR=

bugfiler: bugfiler.c
	${CC} ${CFLAGS} -o bugfiler bugfiler.c

install: bugfiler sendbug.sh bugformat
	install -s bugfiler ${DESTDIR}${LIBDIR}/bugfiler
	install -c sendbug.sh ${DESTDIR}/usr/ucb/sendbug
	install -c -m 644 bugformat ${DESTDIR}/usr/ucb/bugformat
