#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.8	(Berkeley)	6/9/88
#
CFLAGS=	-O
LIBC=	/lib/libc.a
YFLAGS=	-d
OBJS=	awk.lx.o b.o main.o token.o tran.o lib.o run.o parse.o proctab.o \
	freeze.o
SRCS=	awk.lx.c b.c lib.c main.c parse.c proctab.c freeze.c run.c \
	token.c tran.c

all: awk

awk: ${OBJS} awk.g.o ${LIBC}
	${CC} ${CFLAGS} awk.g.o  ${OBJS} -lm -o $@

y.tab.h: awk.g.o

awk.h: y.tab.h
	-cmp -s y.tab.h awk.h || cp y.tab.h awk.h

token.c: awk.h
	ex - < tokenscript

profile: awk.g.o ${OBJS} mon.o
	${CC} -p -i awk.g.o ${OBJS} mon.o -lm

proctab.c: proc
	./proc > proctab.c

proc: awk.h proc.o token.o
	${CC} -o $@ proc.o token.o

clean: FRC
#	use *.o, not ${OBJS}
	rm -f *.o core awk awk.h awk.lx.c y.tab.h proc proctab.c yacc* temp

depend: y.tab.h ${SRCS} FRC
	mkdep ${CFLAGS} ${SRCS}

install:
	install -s -o bin -g bin -m 755 awk ${DESTDIR}/bin/awk

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: proctab.c FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

awk.lx.o: awk.lx.c /usr/include/stdio.h awk.h awk.def
b.o: b.c awk.def /usr/include/stdio.h awk.h
lib.o: lib.c /usr/include/stdio.h awk.def awk.h /usr/include/ctype.h
main.o: main.c /usr/include/stdio.h /usr/include/ctype.h awk.def awk.h
parse.o: parse.c awk.def awk.h /usr/include/stdio.h
proctab.o: proctab.c awk.def
freeze.o: freeze.c /usr/include/stdio.h
run.o: run.c /usr/include/sys/param.h /usr/include/sys/types.h
run.o: /usr/include/signal.h /usr/include/machine/trap.h
run.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h awk.def
run.o: /usr/include/math.h awk.h /usr/include/stdio.h /usr/include/fcntl.h
token.o: token.c awk.h
tran.o: tran.c /usr/include/stdio.h awk.def awk.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
