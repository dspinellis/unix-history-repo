#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	1.4	(Berkeley)	%G%
#
DESTDIR=
CFLAGS=	-O
SRCS=	c20.c c21.c c22.c c23.c
OBJS=	c20.o c21.o c22.o c23.o

all: c2

c2: ${OBJS} ${LIBC}
	${CC} -o $@ ${OBJS}

c23.o:  c23.c
	${CC} ${CFLAGS} -R -c c23.c

c2.h:	tokens
	grep -n '^' tokens>toks
	( echo '/tokens/+1;/^$$/-1d'; \
	echo '/tokens/r toks'; \
	echo '/tokens/+1;/^$$/-1s/^\([0-9]*\):\([A-Z][A-Z0-9]*\)/#define \2	\1/'; \
	echo 'w'; \
	echo 'q'; \
	) | ed - c2.h
	rm toks

clean:
	rm -f *.o *.s c2

cleandir: clean
	rm -f ${MAN} tags .depend

install: ${MAN}
	install -s -o bin -g bin -m 755 c2 ${DESTDIR}/lib/c2

tags: ${SRCS}
	ctags ${SRCS}

depend: ${SRCS}
	mkdep ${CFLAGS} ${SRCS}
