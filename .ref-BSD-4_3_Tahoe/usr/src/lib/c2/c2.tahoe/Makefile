#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	1.3	(Berkeley)	6/6/87
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

clean: FRC
	rm -f *.o *.s c2

install: FRC
	install -s -o bin -g bin -m 755 c2 ${DESTDIR}/lib/c2

tags: FRC
	ctags ${SRCS}

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

c20.o: c20.c c2.h /usr/include/stdio.h /usr/include/ctype.h
c21.o: c21.c c2.h /usr/include/stdio.h /usr/include/ctype.h
c22.o: c22.c c2.h /usr/include/stdio.h /usr/include/ctype.h
c23.o: c23.c c2.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
