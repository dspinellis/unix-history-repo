#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.3	(Berkeley)	6/19/87
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	pow.c gcd.c msqrt.c mdiv.c mout.c mult.c madd.c util.c
OBJS=	pow.o gcd.o msqrt.o mdiv.o mout.o mult.o madd.o util.o

all: libmp.a

libmp.a: ${OBJS}
	ar cr $@ ${OBJS}

clean: FRC
	rm -f ${OBJS} core libmp.a

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -o bin -g bin -m 644 libmp.a ${DESTDIR}/usr/lib/libmp.a
	ranlib ${DESTDIR}/usr/lib/libmp.a

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

pow.o: pow.c /usr/include/mp.h
gcd.o: gcd.c /usr/include/mp.h
msqrt.o: msqrt.c /usr/include/mp.h
mdiv.o: mdiv.c /usr/include/mp.h
mout.o: mout.c /usr/include/stdio.h /usr/include/mp.h
mult.o: mult.c /usr/include/mp.h
madd.o: madd.c /usr/include/mp.h
util.o: util.c /usr/include/stdio.h /usr/include/mp.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
