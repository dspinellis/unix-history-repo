#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	1.4	(Berkeley)	9/20/87
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SFLAGS=	-r
TDEV=	-Pver
TROFF=	ditroff ${TDEV}
SRCS=	fortune.c rnd.c strfile.c unstr.c

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

clean: FRC
	rm -f fortune fortunes fortunes.dat strfile unstr core *.o
	rm -f Oscene Oobscene

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o games -g bin -m 4755 fortune ${DESTDIR}/usr/games
	install -o games -g bin -m 600 fortunes.dat ${DESTDIR}/usr/games/lib

lint: FRC
	lint ${CFLAGS} fortune.c rnd.c
	lint ${CFLAGS} strfile.c rnd.c
	lint ${CFLAGS} unstr.c

tags: FRC
	ctags ${SRCS}

troff: FRC
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

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

fortune.o: fortune.c /usr/include/sys/types.h /usr/include/stdio.h
fortune.o: /usr/include/sys/file.h strfile.h
rnd.o: rnd.c
strfile.o: strfile.c /usr/include/stdio.h /usr/include/ctype.h strfile.h
unstr.o: unstr.c /usr/include/stdio.h strfile.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
