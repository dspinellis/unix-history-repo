#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.3	(Berkeley)	6/18/87
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	lmain.c parser.c sub1.c sub2.c header.c
OBJS=	lmain.o parser.o sub1.o sub2.o header.o

all: lex

lex: ${OBJS} ${LIBC}
	${CC} ${CFLAGS} ${OBJS} -o $@

smallex: ${SRCS} ${LIBC} FRC
	${CC} -DSMALL ${CFLAGS} ${SRCS} -o $@

clean: FRC
	rm -f ${OBJS} core lex smallex parser.c

depend: ${SRCS} FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 lex ${DESTDIR}/usr/bin/lex
	install -c -o bin -g bin -m 755 ncform ${DESTDIR}/usr/lib/lex/ncform
	install -c -o bin -g bin -m 755 nrform ${DESTDIR}/usr/lib/lex/nrform

lint: ${SRCS} FRC
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS} FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

lmain.o: lmain.c ldefs.c /usr/include/stdio.h once.c
parser.o: parser.c ldefs.c /usr/include/stdio.h
sub1.o: sub1.c ldefs.c /usr/include/stdio.h
sub2.o: sub2.c ldefs.c /usr/include/stdio.h
header.o: header.c ldefs.c /usr/include/stdio.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
