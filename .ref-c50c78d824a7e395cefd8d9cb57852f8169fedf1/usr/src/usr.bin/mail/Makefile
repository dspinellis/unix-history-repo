#
# Copyright (c) 1987 Regents of the University of California.
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
#	@(#)Makefile	5.11 (Berkeley) %G%
#
CFLAGS=	-O -R
LIBC=	/lib/libc.a
SRCS=	version.c aux.c cmd1.c cmd2.c cmd3.c cmdtab.c collect.c edit.c fio.c \
	getname.c head.c v7.local.c lex.c list.c main.c names.c popen.c \
	quit.c send.c strings.c temp.c tty.c vars.c
OBJS=	version.o aux.o cmd1.o cmd2.o cmd3.o cmdtab.o collect.o edit.o fio.o \
	getname.o head.o v7.local.o lex.o list.o main.o names.o popen.o \
	quit.o send.o strings.o temp.o tty.o vars.o flsbuf.o
FSRCS=	fmt.c head.c
FOBJS=	fmt.o head.o
MAN=	mail.0 fmt.0

all: mail fmt

mail: ${OBJS} ${LIBC}
	${CC} -o $@ ${OBJS}

fmt: ${FOBJS} ${LIBC}
	${CC} -o $@ ${FOBJS}

clean:
	rm -f ${FOBJS} ${OBJS} mail a.out fmt core

cleandir: clean
	rm -f ${MAN} tags .depend

depend:
	mkdep fmt.c ${SRCS}

install: ${MAN}
	install -s -o bin -g bin -m 755 mail ${DESTDIR}/usr/ucb/mail
	install -s -o bin -g bin -m 755 fmt ${DESTDIR}/usr/ucb/fmt
	install -c -o bin -g bin -m 444 misc/Mail.* ${DESTDIR}/usr/lib
	rm -f ${DESTDIR}/usr/ucb/Mail
	ln ${DESTDIR}/usr/ucb/mail ${DESTDIR}/usr/ucb/Mail
	install -c -o bin -g bin -m 444 mail.0 ${DESTDIR}/usr/man/cat1/mail.0
	install -c -o bin -g bin -m 444 fmt.0 ${DESTDIR}/usr/man/cat1/fmt.0
	rm -f ${DESTDIR}/usr/man/cat1/Mail.0
	ln ${DESTDIR}/usr/man/cat1/mail.0 ${DESTDIR}/usr/man/cat1/Mail.0

lint:
	lint ${CFLAGS} ${SRCS}
	lint ${CFLAGS} fmt.c head.c

tags:
	ctags ${SRCS}
	ctags -a fmt.c
	sort -o tags tags

