#
# Copyright (c) 1989 The Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation,
# advertising materials, and other materials related to such
# distribution and use acknowledge that the software was developed
# by the University of California, Berkeley.  The name of the
# University may not be used to endorse or promote products derived
# from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	1.7 (Berkeley) %G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	fortune.c strfile.c unstr.c
OBJS=
MAN=	fortune.0

all: fortune strfile # unstr

fortune: ${LIBC}
	${CC} ${CFLAGS} -o $@ $@.c

strfile: ${LIBC}
	${CC} ${CFLAGS} -o $@ $@.c

unstr: ${LIBC}
	${CC} ${CFLAGS} -o $@ $@.c

clean:
	rm -f fortune strfile unstr core *.dat

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep -p ${CFLAGS} ${SRCS}

install: ${MAN}
	install -s -o games -g bin -m 4755 fortune ${DESTDIR}/usr/games
	install -c -o bin -g bin -m 444 fortune.0 ${DESTDIR}/usr/man/cat6
	./strfile -rs fortunes
	./strfile -rs startrek
	./strfile -rs zippy
	install -c -o bin -g bin -m 444 fortunes startrek zippy \
	    ${DESTDIR}/usr/share/games/fortune
	install -o bin -g bin -m 444 fortunes.dat startrek.dat zippy.dat \
	    ${DESTDIR}/usr/share/games/fortune
	./strfile -rsx noobscene
	install -c -o bin -g bin -m 444 noobscene \
	    ${DESTDIR}/usr/share/games/fortune/fortunes-o
	install -o bin -g bin -m 444 noobscene.dat \
	    ${DESTDIR}/usr/share/games/fortune/fortunes-o.dat
#	./strfile -rsx fortunes-o.rot
#	install -c -o bin -g bin -m 444 fortunes-o.rot \
#	    ${DESTDIR}/usr/share/games/fortune/fortunes-o
#	install -o bin -g bin -m 444 fortunes-o.rot.dat \
#	    ${DESTDIR}/usr/share/games/fortune/fortunes-o.dat

lint: ${SRCS}
	lint ${CFLAGS} fortune.c
	lint ${CFLAGS} strfile.c
	lint ${CFLAGS} unstr.c

tags: ${SRCS}
	ctags ${SRCS}
