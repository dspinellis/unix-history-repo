#
# Copyright (c) 1988 The Regents of the University of California.
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
# WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	4.8 (Berkeley) %G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	done.c init.c io.c main.c save.c subr.c vocab.c wizard.c
OBJS=	done.o init.o io.o main.o save.o subr.o vocab.o wizard.o
MAN=	adventure.0

advent: ${OBJS} ${LIBC}
	${CC} -n ${CFLAGS} ${OBJS}
	rm -f $@
	echo y | ./a.out

save.o: save.c			# vax C optimizer bug - infinite loop
	${CC} -c save.c

clean:
	rm -f ${OBJS} core advent a.out

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep ${CFLAGS} ${SRCS}

install: ${MAN}
	install -s -o games -g bin -m 700 advent ${DESTDIR}/usr/games/hide/adventure
	(cd ${DESTDIR}/usr/games; rm -f adventure; ln -s dm adventure; chown games.bin adventure)
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat6

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}
