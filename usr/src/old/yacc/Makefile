#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.4	(Berkeley)	%G%
#
CFLAGS=	-O -DWORD32
LIBC=	/lib/libc.a
SRCS=	y1.c y2.c y3.c y4.c
OBJS=	y1.o y2.o y3.o y4.o

all: yacc

yacc: ${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS}

clean: FRC
	rm -f ${OBJS} core yacc

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 yacc ${DESTDIR}/usr/bin/yacc
	install -c -o bin -g bin -m 755 yaccpar ${DESTDIR}/usr/lib/yaccpar

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

y1.o: y1.c dextern /usr/include/stdio.h /usr/include/ctype.h files
y2.o: y2.c dextern /usr/include/stdio.h /usr/include/ctype.h files
y3.o: y3.c dextern /usr/include/stdio.h /usr/include/ctype.h files
y4.o: y4.c dextern /usr/include/stdio.h /usr/include/ctype.h files

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
