#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.4	(Berkeley)	6/19/87
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	enroll.c lib.c xget.c xsend.c
OBJS=	enroll.o lib.o xget.o xsend.o

all: enroll xsend xget

enroll: enroll.o lib.o ${LIBC}
	${CC} enroll.o lib.o -lmp -o $@

xget: xget.o lib.o ${LIBC}
	${CC} xget.o lib.o -lmp -o $@

xsend: xsend.o lib.o ${LIBC}
	${CC} xsend.o lib.o -lmp -o $@

clean: FRC
	rm -f ${OBJS} core enroll xsend xget

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 enroll ${DESTDIR}/usr/bin/enroll
	install -s -o bin -g bin -m 755 xget ${DESTDIR}/usr/bin/xget
	install -s -o bin -g bin -m 755 xsend ${DESTDIR}/usr/bin/xsend

lint: FRC
	lint ${CFLAGS} enroll.c lib.c
	lint ${CFLAGS} xget.c lib.c
	lint ${CFLAGS} xsend.c lib.c

tags: FRC
	ctags enroll.c lib.c
	ctags -a xget.c
	ctags -a xsend.c
	sort -o tags tags

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

enroll.o: enroll.c xmail.h /usr/include/stdio.h /usr/include/mp.h
enroll.o: /usr/include/pwd.h /usr/include/sys/types.h
lib.o: lib.c xmail.h /usr/include/stdio.h /usr/include/mp.h
xget.o: xget.c xmail.h /usr/include/stdio.h /usr/include/mp.h
xget.o: /usr/include/sys/types.h /usr/include/sys/dir.h /usr/include/ctype.h
xget.o: /usr/include/pwd.h /usr/include/sys/stat.h
xsend.o: xsend.c xmail.h /usr/include/stdio.h /usr/include/mp.h
xsend.o: /usr/include/sys/types.h /usr/include/pwd.h /usr/include/sys/stat.h
xsend.o: /usr/include/sys/dir.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
