#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.2 (Berkeley) 10/26/87
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	ey0.c ey1.c ey2.c ey3.c ey4.c ey5.c
OBJS=	ey0.o ey1.o ey2.o ey3.o ey4.o ey5.o

all: eyacc

eyacc: ${LIBC} ${OBJS}
	${CC} ${CFLAGS} -o $@ -z ${OBJS}

clean: FRC
	rm -f ${OBJS} core eyacc

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
#	install -s -o bin -g bin -m 755 eyacc ${DESTDIR}/usr/ucb/eyacc

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

ey0.o: ey0.c /usr/include/stdio.h
ey1.o: ey1.c ey.h /usr/include/stdio.h
ey2.o: ey2.c ey.h /usr/include/stdio.h
ey3.o: ey3.c ey.h /usr/include/stdio.h
ey4.o: ey4.c ey.h /usr/include/stdio.h
ey5.o: ey5.c /usr/include/stdio.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
