#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.6	(Berkeley)	%G%
#
CFLAGS=	-O -DASCARCH -I. -I/usr/src/bin/make 
LIBC=	/lib/libc.a
SRCS=	ident.c main.c doname.c misc.c files.c dosys.c gram.c
OBJS=	ident.o main.o doname.o misc.o files.o dosys.o gram.o

all:	make

make:	${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS}

clean: FRC
	rm -f ${OBJS} core make gram.c

depend: gram.c FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 make ${DESTDIR}/bin/make

lint: gram.c FRC
	lint ${CFLAGS} ${SRCS}

tags: gram.c FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

ident.o: ident.c
main.o: main.c defs /usr/include/stdio.h /usr/include/ctype.h
main.o: /usr/include/sys/param.h /usr/include/sys/types.h /usr/include/signal.h
main.o: /usr/include/machine/machparam.h /usr/include/sys/dir.h
main.o: /usr/include/signal.h /usr/include/sys/stat.h
doname.o: doname.c defs /usr/include/stdio.h /usr/include/ctype.h
doname.o: /usr/include/sys/param.h /usr/include/sys/types.h
doname.o: /usr/include/signal.h /usr/include/machine/machparam.h
doname.o: /usr/include/sys/dir.h /usr/include/strings.h /usr/include/signal.h
misc.o: misc.c defs /usr/include/stdio.h /usr/include/ctype.h
misc.o: /usr/include/sys/param.h /usr/include/sys/types.h /usr/include/signal.h
misc.o: /usr/include/machine/machparam.h /usr/include/sys/dir.h
files.o: files.c /usr/include/fcntl.h defs /usr/include/stdio.h
files.o: /usr/include/ctype.h /usr/include/sys/param.h /usr/include/sys/types.h
files.o: /usr/include/signal.h /usr/include/machine/machparam.h
files.o: /usr/include/sys/dir.h /usr/include/sys/stat.h /usr/include/ar.h
files.o: /usr/include/a.out.h /usr/include/sys/exec.h /usr/include/sys/stat.h
dosys.o: dosys.c defs /usr/include/stdio.h /usr/include/ctype.h
dosys.o: /usr/include/sys/param.h /usr/include/sys/types.h
dosys.o: /usr/include/signal.h /usr/include/machine/machparam.h
dosys.o: /usr/include/sys/dir.h /usr/include/signal.h /usr/include/errno.h
dosys.o: /usr/include/sys/stat.h
gram.o: gram.c defs /usr/include/stdio.h /usr/include/ctype.h
gram.o: /usr/include/sys/param.h /usr/include/sys/types.h /usr/include/signal.h
gram.o: /usr/include/machine/machparam.h /usr/include/sys/dir.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
