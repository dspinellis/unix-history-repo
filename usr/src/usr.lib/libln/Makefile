#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.3	(Berkeley)	6/19/87
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	allprint.c main.c reject.c yyless.c yywrap.c
OBJS=	allprint.o main.o reject.o yyless.o yywrap.o

all: libln.a

libln.a: ${OBJS}
	ar rc $@ ${OBJS}

clean: FRC
	rm -f ${OBJS} core libln.a

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -o bin -g bin -m 644 libln.a ${DESTDIR}/usr/lib/libln.a
	rm -f ${DESTDIR}/usr/lib/libl.a
	ln ${DESTDIR}/usr/lib/libln.a ${DESTDIR}/usr/lib/libl.a
	ranlib ${DESTDIR}/usr/lib/libln.a

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

allprint.o: allprint.c /usr/include/stdio.h
main.o: main.c /usr/include/stdio.h
reject.o: reject.c /usr/include/stdio.h
yyless.o: yyless.c
yywrap.o: yywrap.c

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
