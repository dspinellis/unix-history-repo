#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.3	(Berkeley)	6/18/87
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	t0.c t1.c t2.c t3.c t4.c t5.c t6.c t7.c t8.c t9.c tb.c tc.c te.c \
	tf.c tg.c ti.c tm.c ts.c tt.c tu.c tv.c
OBJS=	t0.o t1.o t2.o t3.o t4.o t5.o t6.o t7.o t8.o t9.o tb.o tc.o te.o \
	tf.o tg.o ti.o tm.o ts.o tt.o tu.o tv.o

all: tbl

tbl: ${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS}

clean: FRC
	rm -f ${OBJS} core tbl

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 tbl ${DESTDIR}/usr/bin/tbl

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
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
FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

t0.o: t0.c t..c /usr/include/stdio.h /usr/include/ctype.h
t1.o: t1.c t..c /usr/include/stdio.h /usr/include/ctype.h /usr/include/signal.h
t1.o: /usr/include/machine/trap.h
t2.o: t2.c t..c /usr/include/stdio.h /usr/include/ctype.h
t3.o: t3.c t..c /usr/include/stdio.h /usr/include/ctype.h
t4.o: t4.c t..c /usr/include/stdio.h /usr/include/ctype.h
t5.o: t5.c t..c /usr/include/stdio.h /usr/include/ctype.h
t6.o: t6.c t..c /usr/include/stdio.h /usr/include/ctype.h
t7.o: t7.c t..c /usr/include/stdio.h /usr/include/ctype.h
t8.o: t8.c t..c /usr/include/stdio.h /usr/include/ctype.h
t9.o: t9.c t..c /usr/include/stdio.h /usr/include/ctype.h
tb.o: tb.c t..c /usr/include/stdio.h /usr/include/ctype.h
tc.o: tc.c t..c /usr/include/stdio.h /usr/include/ctype.h
te.o: te.c t..c /usr/include/stdio.h /usr/include/ctype.h
tf.o: tf.c t..c /usr/include/stdio.h /usr/include/ctype.h
tg.o: tg.c t..c /usr/include/stdio.h /usr/include/ctype.h
ti.o: ti.c t..c /usr/include/stdio.h /usr/include/ctype.h
tm.o: tm.c t..c /usr/include/stdio.h /usr/include/ctype.h
ts.o: ts.c
tt.o: tt.c t..c /usr/include/stdio.h /usr/include/ctype.h
tu.o: tu.c t..c /usr/include/stdio.h /usr/include/ctype.h
tv.o: tv.c t..c /usr/include/stdio.h /usr/include/ctype.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
