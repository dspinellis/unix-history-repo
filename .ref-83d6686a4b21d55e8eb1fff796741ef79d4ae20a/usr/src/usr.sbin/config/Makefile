#
# Copyright (c) 1988 Regents of the University of California.
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
#	@(#)Makefile	5.9 (Berkeley) %G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
LIBS=	-ll
SRCS=	y.tab.c main.c lex.yy.c mkioconf.c mkmakefile.c \
	mkglue.c mkheaders.c mkswapconf.c
OBJS=	y.tab.o main.o lex.yy.o mkioconf.o mkmakefile.o \
	mkglue.o mkheaders.o mkswapconf.o
MAN=	config.0

all: config

config: ${OBJS} ${LIBC}
	${CC} -o $@ ${OBJS} ${LIBS}

lex.yy.c: config.l
	lex config.l

y.tab.h: 
	yacc -d config.y

y.tab.c: config.y
	yacc config.y

clean:
	rm -f ${OBJS} lex.yy.c y.tab.c y.tab.h config core

cleandir: clean
	rm -f ${MAN} tags .depend

depend: y.tab.h ${SRCS}
	mkdep ${CFLAGS} ${SRCS}

install: ${MAN}
	install -s -o bin -g bin -m 755 config ${DESTDIR}/usr/sbin
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat8

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}
