#
# Copyright (c) 1987 The Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation,
# advertising materials, and other materials related to such
# distribution and use acknowledge that the software was developed
# by the University of California, Berkeley.  The name of the
# University may not be used to endorse or promote products derived
# from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	5.14 (Berkeley) %G%
#
# C Shell with process control; VM/UNIX VAX Makefile
# Bill Joy UC Berkeley; Jim Kulp IIASA, Austria
#
# To profile, put -DPROF in DEFS and -pg in CFLAGS, and recompile.

DEFS=	-DTELL -DVFORK -DFILEC
CFLAGS=	${DEFS} -I. -O
XSTR=	/usr/ucb/xstr
MAN=	csh.0

SRCS=	alloc.c doprnt.c sh.c sh.char.c sh.dir.c sh.dol.c sh.err.c \
	sh.exec.c sh.exp.c sh.file.c sh.func.c sh.glob.c sh.hist.c \
	sh.init.c sh.lex.c sh.misc.c sh.parse.c sh.print.c sh.proc.c \
	sh.sem.c sh.set.c sh.time.c
OBJS=	alloc.o doprnt.o sh.o sh.char.o sh.dir.o sh.dol.o sh.err.o \
	sh.exec.o sh.exp.o sh.file.o sh.func.o sh.glob.o sh.hist.o \
	sh.init.o sh.lex.o sh.misc.o sh.parse.o sh.print.o sh.proc.o \
	sh.sem.o sh.set.o sh.time.o

# Special massaging of C files for sharing of strings
.c.o:
	${CC} -E ${CFLAGS} $*.c | ${XSTR} -c -
	@${CC} -c ${CFLAGS} x.c
	@mv -f x.o $*.o
	@rm -f x.c

all: csh

# strings.o must be last since it can change when previous files compile
csh: ${OBJS} strings.o
	${CC} ${CFLAGS} ${OBJS} strings.o -o $@

# strings.o, sh.init.o, and sh.char.o are specially processed to be shared
strings.o: strings
	${XSTR}
	${CC} -c -R xs.c
	mv -f xs.o strings.o
	rm -f xs.c

sh.char.o sh.init.o:
	${CC} -E ${CFLAGS} $*.c | ${XSTR} -c -
	${CC} ${CFLAGS} -c -R x.c
	mv -f x.o $*.o
	rm -f x.c

clean:
	rm -f ${OBJS} core csh strings strings.o x.c x.o xs.c xs.o

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep ${CFLAGS} ${SRCS}

install: ${MAN}
	install -s -o bin -g bin -m 755 csh ${DESTDIR}/bin/csh
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat1
	rm -f ${DESTDIR}/usr/man/cat1/limit.0
	ln ${DESTDIR}/usr/man/cat1/csh.0 ${DESTDIR}/usr/man/cat1/limit.0

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags:
	ctags ${SRCS}
