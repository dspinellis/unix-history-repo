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
#	@(#)Makefile	5.19 (Berkeley) 10/10/88
#
CFLAGS=	-O
RES=
LIBC=	/lib/libc.a
LIBS=	-ll ${RES}
CSRCS=	main.c getinfo.c debug.c send.c skip.c list.c subr.c
SRCS=	${CSRCS} commands.c
OBJS=	main.o getinfo.o debug.o send.o skip.o list.o subr.o commands.o
MAN=	nslookup.0

nslookup: ${OBJS} ${LIBC}
	${CC} -o $@ ${OBJS} ${LIBS}

clean:
	rm -f ${OBJS} core nslookup commands.c nslookup.0 lex.yy.c lex.yy.o

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep ${CFLAGS} ${SRCS}

install: ${MAN}
	install -s -o bin -g bin -m 755 nslookup ${DESTDIR}/usr/local/nslookup
	install -c -o bin -g bin -m 444 nslookup.help ${DESTDIR}/usr/local/nslookup.help
	install -c -o bin -g bin -m 444 nslookup.0 ${DESTDIR}/usr/local/man/cat1/nslookup.0

lint: ${SRCS}
	lint ${SRCS}

tags: ${CSRCS}
	ctags ${CSRCS}
