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
#	@(#)Makefile	4.5 (Berkeley) %G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	t0.c t1.c t2.c t3.c t4.c t5.c t6.c t7.c t8.c t9.c tb.c tc.c te.c \
	tf.c tg.c ti.c tm.c ts.c tt.c tu.c tv.c
OBJS=	t0.o t1.o t2.o t3.o t4.o t5.o t6.o t7.o t8.o t9.o tb.o tc.o te.o \
	tf.o tg.o ti.o tm.o ts.o tt.o tu.o tv.o
MAN=	tbl.0

all: tbl

tbl: ${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS}

clean:
	rm -f ${OBJS} core tbl

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep ${CFLAGS} ${SRCS}

install: ${MAN}
	install -s -o bin -g bin -m 755 tbl ${DESTDIR}/usr/bin
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat1

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}

test:
	sh compr sample01
	sh compr sample02
	sh compr sample03
	sh compr sample04
	sh compr sample05
	sh compr sample06
	sh compr sample07
	sh compr sample08
	sh compeq sample09
	sh compeq sample10
	sh compeq sample11
	sh compr sample12
	sh compr sample13
	sh compr sample14
	sh compr sample15
	sh compr sample16
	sh compr sample17
	sh compr sample18
	sh compr sample19
	sh compr sample20
	sh compr sample21
	sh compr sample22
	sh compr sample23
	sh compms sample24
	sh compr sample25
	sh compr sample26
	sh compr sample27
	sh compr sample28
	sh compr sample29
	sh compr sample30
	sh compr sample31
	sh compr sample32
	sh compr sample33
	sh compms sample34
	sh compeq sample35
	sh compeq sample36
	sh compnr sample37
	sh compr sample38
	sh compr sample39
	sh compr sample40
	sh compr sample41
	sh compr sample42
	sh compr sample43
	sh compr sample44
	sh compms sample45
	sh compms sample46
	sh compr sample47
	sh compnr sample48
	sh compeq /usr/doc/mel/tables

ichk: /usr/bin/tbl
	/usr/bin/tbl sample1 >junk1
	cmp -s junk1 out1
