#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	1.6	(Berkeley)	%G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SFLAGS=	-r
TDEV=	-Pver
TROFF=	ditroff ${TDEV}
SRCS=	fortune.c rnd.c strfile.c unstr.c
MAN=	fortune.0

all: fortune strfile unstr fortunes.dat

fortune: fortune.o rnd.o ${LIBC}
	${CC} ${CFLAGS} -o $@ fortune.o rnd.o

strfile: strfile.o rnd.o ${LIBC}
	${CC} ${CFLAGS} -o $@ strfile.o rnd.o

unstr: unstr.o ${LIBC}
	${CC} ${CFLAGS} -o $@ unstr.o

fortunes.dat: fortunes strfile
	./strfile ${SFLAGS} fortunes

fortunes: scene obscene
	(cat scene; echo "%-"; cat obscene) > fortunes

clean:
	rm -f fortune fortunes fortunes.dat strfile unstr core *.o
	rm -f Oscene Oobscene

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep ${CFLAGS} ${SRCS}

install: ${MAN}
	install -s -o games -g bin -m 4755 fortune ${DESTDIR}/usr/games
	install -o games -g bin -m 600 fortunes.dat ${DESTDIR}/usr/games/lib
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat6

lint: ${SRCS}
	lint ${CFLAGS} fortune.c rnd.c
	lint ${CFLAGS} strfile.c rnd.c
	lint ${CFLAGS} unstr.c

tags: ${SRCS}
	ctags ${SRCS}

troff:
	./Do_troff scene ${TROFF}
	./Do_troff obscene ${TROFF}

sort: sort.scene sort.obscene

sort.scene: strfile unstr
	strfile -oi scene
	mv scene Oscene
	unstr -o scene

sort.obscene: strfile unstr
	strfile -oi obscene
	mv obscene Oobscene
	unstr -o obscene
