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
#	@(#)Makefile	4.10 (Berkeley) %G%
#
# Flag BSD4_2 means 4.2 or later.
CFLAGS=	-O -DBSD4_2
LIBC=	/lib/libc.a
LLIB=	${DESTDIR}/usr/lib/learn/bin
SRCS=	copy.c dounit.c getlesson.c learn.c list.c mem.c makpipe.c maktee.c \
	mysys.c selsub.c selunit.c start.c whatnow.c wrapup.c
OBJS=	copy.o dounit.o getlesson.o learn.o list.o mem.o makpipe.o maktee.o \
	mysys.o selsub.o selunit.o start.o whatnow.o wrapup.o
LESSONS=C editor eqn files macros morefiles
MAN=	learn.0

all: learn lrntee lcount getline.o getnum.o

learn: ${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS}

lcount lrntee: ${LIBC}
	${CC} ${CFLAGS} -o $@ $@.c

clean:
	rm -f ${OBJS} core learn lrntee lcount getline.o getnum.o

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep -p ${CFLAGS} lcount.c lrntee.c
	mkdep -a ${CFLAGS} ${SRCS} getline.c getnum.c

install: ${MAN}
	install -c -s -o bin -g bin -m 755 learn ${DESTDIR}/usr/bin/learn
	install -s -o bin -g bin -m 755 learn ${LLIB}/learn
	install -s -o bin -g bin -m 755 lrntee ${LLIB}/lrntee
	install -s -o bin -g bin -m 755 lcount ${LLIB}/lcount
	install -o bin -g bin -m 644 getline.o ${LLIB}/getline.o
	install -o bin -g bin -m 644 getnum.o ${LLIB}/getnum.o
	cd ${LLIB}; rm -f ../C/get*.o; ln getline.o getnum.o ../C
	install -c -o bin -g bin -m 644 getline.c ${LLIB}/../C/getline.c
	install -c -o bin -g bin -m 644 getnum.c ${LLIB}/../C/getnum.c
	chmod 755 ${LLIB}/../*/Init
	-[ -d ${DESTDIR}/usr/lib/learn ] || mkdir ${DESTDIR}/usr/lib/learn
	-[ -d ${DESTDIR}/usr/lib/learn/log ] || mkdir ${DESTDIR}/usr/lib/learn/log
	-[ -d ${DESTDIR}/usr/lib/learn/bin ] || mkdir ${DESTDIR}/usr/lib/learn/bin
	chmod 755 ${DESTDIR}/usr/lib/learn ${DESTDIR}/usr/lib/learn/log \
		${DESTDIR}/usr/lib/learn/bin
	chown root.staff ${DESTDIR}/usr/lib/learn ${DESTDIR}/usr/lib/learn/log \
		${DESTDIR}/usr/lib/learn/bin
	install -c -o bin -g bin -m 644 learnlib/Linfo ${DESTDIR}/usr/lib/learn/Linfo
	install -c -o bin -g bin -m 644 learnlib/Xinfo ${DESTDIR}/usr/lib/learn/Xinfo
	cd learnlib; tar cf - ${LESSONS} | (cd ${DESTDIR}/usr/lib/learn; tar xpf -);
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat1

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}
	lint ${CFLAGS} lcount.c
	lint ${CFLAGS} lrntee.c
	lint ${CFLAGS} getline.c getnum.c

tags: ${SRCS}
	ctags ${SRCS}
	ctags -a lcount.c
	ctags -a lrntee.c
	ctags -a getline.c getnum.c
	sort -o tags tags
