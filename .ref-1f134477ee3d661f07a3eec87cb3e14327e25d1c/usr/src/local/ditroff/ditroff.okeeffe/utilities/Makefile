#
#	Makefile	(CWI)	1.1	87/03/31
#
CFLAGS=-O
DESTDIR=
BINDIR=/usr/local
HFILES= ../dev.h
ALL= makedev dumpdev

all:	${ALL}

makedev:	makedev.c ${HFILES}
	${CC} -o makedev ${CFLAGS} makedev.c

dumpdev:	dumpdev.c ${HFILES}
	${CC} -o dumpdev ${CFLAGS} dumpdev.c

install:	all
	install -s makedev ${DESTDIR}${BINDIR}/makedev
	install -s dumpdev ${DESTDIR}${BINDIR}/dumpdev

clean:
	rm -f make.out ${ALL}
