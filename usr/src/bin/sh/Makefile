#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.7	(Berkeley)	6/2/87
#
CFLAGS=	-O -w
LIBC=	/lib/libc.a
SRCS=	setbrk.c builtin.c blok.c stak.c cmd.c fault.c main.c word.c \
	string.c name.c args.c xec.c service.c error.c io.c print.c \
	macro.c expand.c ctype.c msg.c
OBJS=	setbrk.o builtin.o blok.o stak.o cmd.o fault.o main.o word.o \
	string.o name.o args.o xec.o service.o error.o io.o print.o \
	macro.o expand.o ctype.o msg.o

all: sh

sh:	${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS}

clean: FRC
	rm -f ${OBJS} core sh

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 sh ${DESTDIR}/bin/sh

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

setbrk.o: setbrk.c defs.h mac.h mode.h name.h stak.h /usr/include/setjmp.h
setbrk.o: brkincr.h ctype.h
builtin.o: builtin.c
blok.o: blok.c defs.h mac.h mode.h name.h stak.h /usr/include/setjmp.h
blok.o: brkincr.h ctype.h
stak.o: stak.c defs.h mac.h mode.h name.h stak.h /usr/include/setjmp.h
stak.o: brkincr.h ctype.h
cmd.o: cmd.c defs.h mac.h mode.h name.h stak.h /usr/include/setjmp.h brkincr.h
cmd.o: ctype.h sym.h
fault.o: fault.c defs.h mac.h mode.h name.h stak.h /usr/include/setjmp.h
fault.o: brkincr.h ctype.h
main.o: main.c defs.h mac.h mode.h name.h stak.h /usr/include/setjmp.h
main.o: brkincr.h ctype.h sym.h timeout.h /usr/include/sys/types.h
main.o: /usr/include/sys/stat.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
main.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
main.o: /usr/include/signal.h /usr/include/machine/trap.h
word.o: word.c defs.h mac.h mode.h name.h stak.h /usr/include/setjmp.h
word.o: brkincr.h ctype.h sym.h
string.o: string.c defs.h mac.h mode.h name.h stak.h /usr/include/setjmp.h
string.o: brkincr.h ctype.h
name.o: name.c defs.h mac.h mode.h name.h stak.h /usr/include/setjmp.h
name.o: brkincr.h ctype.h
args.o: args.c defs.h mac.h mode.h name.h stak.h /usr/include/setjmp.h
args.o: brkincr.h ctype.h
xec.o: xec.c defs.h mac.h mode.h name.h stak.h /usr/include/setjmp.h brkincr.h
xec.o: ctype.h sym.h
service.o: service.c defs.h mac.h mode.h name.h stak.h /usr/include/setjmp.h
service.o: brkincr.h ctype.h
error.o: error.c defs.h mac.h mode.h name.h stak.h /usr/include/setjmp.h
error.o: brkincr.h ctype.h
io.o: io.c defs.h mac.h mode.h name.h stak.h /usr/include/setjmp.h brkincr.h
io.o: ctype.h
print.o: print.c defs.h mac.h mode.h name.h stak.h /usr/include/setjmp.h
print.o: brkincr.h ctype.h
macro.o: macro.c defs.h mac.h mode.h name.h stak.h /usr/include/setjmp.h
macro.o: brkincr.h ctype.h sym.h
expand.o: expand.c defs.h mac.h mode.h name.h stak.h /usr/include/setjmp.h
expand.o: brkincr.h ctype.h /usr/include/sys/param.h /usr/include/sys/types.h
expand.o: /usr/include/signal.h /usr/include/machine/trap.h
expand.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
expand.o: /usr/include/sys/stat.h /usr/include/sys/dir.h
ctype.o: ctype.c defs.h mac.h mode.h name.h stak.h /usr/include/setjmp.h
ctype.o: brkincr.h ctype.h
msg.o: msg.c defs.h mac.h mode.h name.h stak.h /usr/include/setjmp.h brkincr.h
msg.o: ctype.h sym.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
