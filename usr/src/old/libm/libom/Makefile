#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.4	(Berkeley)	%G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	asin.c atan.c hypot.c jn.c j0.c j1.c pow.c gamma.c \
	log.c sin.c sqrt.c tan.c tanh.c sinh.c erf.c exp.c floor.c
OBJS=	asin.o atan.o hypot.o jn.o j0.o j1.o pow.o gamma.o \
	log.o sin.o sqrt.o tan.o tanh.o sinh.o erf.o exp.o floor.o

.c.o:
	${CC} -p ${CFLAGS} -c $*.c
	-ld -X -r $*.o
	mv a.out profiled/$*.o
	${CC} ${CFLAGS} -c $*.c
	-ld -x -r $*.o
	mv a.out $*.o

all: libom.a libom_p.a

libom.a libom_p.a: ${OBJS}
	cd profiled; ar cru ../libom_p.a ${OBJS}
	ar cru libom.a ${OBJS}

clean: FRC
	rm -f ${OBJS} core libom.a libom_p.a

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	rm -f ${DESTDIR}/usr/lib/libnm.a ${DESTDIR}/usr/lib/libnm_p.a
	install -o bin -g bin -m 644 libom.a ${DESTDIR}/usr/lib/libom.a
	ln ${DESTDIR}/usr/lib/libom.a ${DESTDIR}/usr/lib/libnm.a
	ranlib ${DESTDIR}/usr/lib/libom.a
	install -o bin -g bin -m 644 libom_p.a ${DESTDIR}/usr/lib/libom_p.a
	ln ${DESTDIR}/usr/lib/libom_p.a ${DESTDIR}/usr/lib/libnm_p.a
	ranlib ${DESTDIR}/usr/lib/libom_p.a

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

asin.o: asin.c /usr/include/errno.h
atan.o: atan.c
hypot.o: hypot.c
jn.o: jn.c /usr/include/math.h /usr/include/errno.h
j0.o: j0.c /usr/include/math.h /usr/include/errno.h
j1.o: j1.c /usr/include/math.h /usr/include/errno.h
pow.o: pow.c /usr/include/errno.h
gamma.o: gamma.c /usr/include/errno.h /usr/include/math.h
log.o: log.c /usr/include/errno.h /usr/include/math.h
sin.o: sin.c
sqrt.o: sqrt.c /usr/include/errno.h
tan.o: tan.c /usr/include/errno.h /usr/include/math.h
tanh.o: tanh.c
sinh.o: sinh.c
erf.o: erf.c
exp.o: exp.c /usr/include/errno.h /usr/include/math.h
floor.o: floor.c

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
