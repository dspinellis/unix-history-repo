#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.7	(Berkeley)	11/16/87
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	diction.c nwords.c end.c part.c pscan.c outp.c
OBJS=	nwords.o end.o part.o pscan.o outp.o

all: dprog style1 style2 style3

dprog: diction.c ${LIBC}
	${CC} ${CFLAGS} -DDICT=\"/usr/lib/dict.d\" diction.c -o $@

style1: nwords.o ${LIBC}
	${CC} nwords.o -o $@ -ll

style2: end.o ${LIBC}
	${CC} end.o -o $@ -ll

style3: part.o pscan.o outp.o ${LIBC}
	${CC} part.o pscan.o outp.o -o $@ -ll

clean: FRC
	rm -f ${OBJS} part.c nwords.c end.c style1 style2 style3 dprog

depend: ${SRCS} FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 style1 ${DESTDIR}/usr/lib
	install -s -o bin -g bin -m 755 style2 ${DESTDIR}/usr/lib
	install -s -o bin -g bin -m 755 style3 ${DESTDIR}/usr/lib
	install -s -o bin -g bin -m 755 dprog ${DESTDIR}/usr/lib
	install -c -o bin -g bin -m 755 style.sh ${DESTDIR}/usr/bin/style
	install -c -o bin -g bin -m 755 diction.sh ${DESTDIR}/usr/bin/diction
	install -c -o bin -g bin -m 755 explain.sh ${DESTDIR}/usr/bin/explain
	install -c -o bin -g bin -m 755 dict.d ${DESTDIR}/usr/lib
	install -c -o bin -g bin -m 755 explain.d ${DESTDIR}/usr/lib
#	ln ${DESTDIR}/usr/bin/explain ${DESTDIR}/usr/bin/suggest

lint: ${SRCS} FRC
	lint ${CFLAGS} nwords.c
	lint ${CFLAGS} end.c
	lint ${CFLAGS} part.c pscan.c outp.c

tags: ${SRCS} FRC
	ctags nwords.c
	ctags -a end.c
	ctags -a part.c pscan.c outp.c
	sort -o tags tags

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

diction.o: diction.c /usr/include/stdio.h /usr/include/ctype.h
nwords.o: nwords.c /usr/include/stdio.h /usr/include/stdio.h
nwords.o: /usr/include/ctype.h names.h nhash.c dict.c ydict.c abbrev.c
end.o: end.c /usr/include/stdio.h /usr/include/stdio.h /usr/include/ctype.h
end.o: names.h ehash.c edict.c
part.o: part.c /usr/include/stdio.h style.h names.h conp.h
pscan.o: pscan.c names.h conp.h
outp.o: outp.c /usr/include/stdio.h /usr/include/ctype.h style.h names.h conp.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
