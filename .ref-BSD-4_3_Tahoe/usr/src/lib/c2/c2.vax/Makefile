#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.6	(Berkeley)	6/6/87
#
CFLAGS=	-DCOPYCODE -DC2 -O
ASDIR=	../../../bin/as/as.${MACHINE}
SRCS=	c20.c c21.c c22.c
OBJS=	c20.o c21.o c22.o

all: c2

c2: ${OBJS} ${LIBC}
	${CC} -o $@ ${OBJS}

c22.o: c22.c c2.h instrs.c2
	${CC} ${CFLAGS} -R -c c22.c

instrs.c2: ${ASDIR}/instrs
	(echo FLAVOR C2 ; cat ${ASDIR}/instrs) | awk -f ${ASDIR}/instrs > instrs.c2

clean:
	rm -f *.o *.s c2

install:
	install -o bin -g bin -m 755 -s c2 ${DESTDIR}/lib/c2

tags:
	ctags ${SRCS}

depend:
	mkdep ${CFLAGS} ${SRCS}

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

c20.o: c20.c
c20.o:c2.h
c20.o: /usr/include/stdio.h /usr/include/ctype.h /usr/include/sys/types.h
c21.o: c21.c
c21.o:c2.h
c21.o: /usr/include/stdio.h /usr/include/ctype.h
c22.o: c22.c
c22.o:c2.h
c22.o:./instrs.c2

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
