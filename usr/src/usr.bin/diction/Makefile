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
#	@(#)Makefile	4.8 (Berkeley) %G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	diction.c nwords.c end.c part.c pscan.c outp.c
OBJS=	nwords.o end.o part.o pscan.o outp.o
MAN=	diction.0 style.0

all: dprog style1 style2 style3

dprog: diction.c ${LIBC}
	${CC} ${CFLAGS} -DDICT=\"/usr/lib/dict.d\" diction.c -o $@

style1: nwords.o ${LIBC}
	${CC} nwords.o -o $@ -ll

style2: end.o ${LIBC}
	${CC} end.o -o $@ -ll

style3: part.o pscan.o outp.o ${LIBC}
	${CC} part.o pscan.o outp.o -o $@ -ll

clean:
	rm -f ${OBJS} part.c nwords.c end.c style1 style2 style3 dprog

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS} FRC
	mkdep ${CFLAGS} ${SRCS}

install: ${MAN}
	install -s -o bin -g bin -m 755 style1 ${DESTDIR}/usr/lib
	install -s -o bin -g bin -m 755 style2 ${DESTDIR}/usr/lib
	install -s -o bin -g bin -m 755 style3 ${DESTDIR}/usr/lib
	install -s -o bin -g bin -m 755 dprog ${DESTDIR}/usr/lib
	install -c -o bin -g bin -m 755 style.sh ${DESTDIR}/usr/bin/style
	install -c -o bin -g bin -m 755 diction.sh ${DESTDIR}/usr/bin/diction
	install -c -o bin -g bin -m 755 explain.sh ${DESTDIR}/usr/bin/explain
	install -c -o bin -g bin -m 755 dict.d ${DESTDIR}/usr/lib
	install -c -o bin -g bin -m 755 explain.d ${DESTDIR}/usr/lib
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat1
	rm -f ${DESTDIR}/usr/man/cat1/explain.0
	ln ${DESTDIR}/usr/man/cat1/diction.0 ${DESTDIR}/usr/man/cat1/explain.0
#	ln ${DESTDIR}/usr/bin/explain ${DESTDIR}/usr/bin/suggest

lint: ${SRCS}
	lint ${CFLAGS} nwords.c
	lint ${CFLAGS} end.c
	lint ${CFLAGS} part.c pscan.c outp.c

tags: ${SRCS}
	ctags nwords.c
	ctags -a end.c
	ctags -a part.c pscan.c outp.c
	sort -o tags tags
