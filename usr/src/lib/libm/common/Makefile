#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	1.1	(ucb.elefunt)	%G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	atan2.c cabs.c trig.c
OBJS=	atan2.o cabs.o trig.o

.c.o:
	${CC} -p ${CFLAGS} -c $*.c
	-ld -X -r $*.o
	mv a.out ../profiled/$*.o
	${CC} ${CFLAGS} -c $*.c
	-ld -x -r $*.o
	mv a.out $*.o

all: ../libm.a ../libm_p.a

../libm.a ../libm_p.a: ${OBJS}
	cd ../profiled; ar cru ../libm_p.a ${OBJS}
	ar cru ../libm.a ${OBJS}

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

atan2.o: atan2.c
cabs.o: cabs.c
trig.o: trig.c

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
