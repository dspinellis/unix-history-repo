#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.8	(Berkeley)	%G%
#
CFLAGS=	-DCOPYCODE -DC2 -O
ASDIR=	../../../usr.bin/as/as.${MACHINE}
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

cleandir: clean
	rm -f ${MAN} tags .depend

install: ${MAN}
	install -o bin -g bin -m 755 -s c2 ${DESTDIR}/usr/libexec/c2

tags: ${SRCS}
	ctags ${SRCS}

depend: ${SRCS}
	mkdep ${CFLAGS} ${SRCS}

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}

