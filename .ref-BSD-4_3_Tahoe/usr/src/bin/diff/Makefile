#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.3	(Berkeley)	6/29/87
#
DIFF=	/bin/diff
DIFFH=	/usr/lib/diffh
PR=	/bin/pr
CFLAGS=	-O -DDIFF='"${DIFF}"' -DDIFFH='"${DIFFH}"' -DPR='"${PR}"'
LIBC=	/lib/libc.a
SRCS=	diff.c diffdir.c diffreg.c
OBJS=	diff.o diffdir.o diffreg.o
HSRCS=	diffh.c
HOBJS=	diffh.o

all: diff diffh

diff: ${OBJS} ${LIBC}
	${CC} ${CFLAGS} -o $@ ${OBJS}

diffh: ${HOBJS} ${LIBC}
	${CC} ${CFLAGS} -o $@ ${HOBJS}

clean: FRC
	rm -f ${HOBJS} ${OBJS} core diff diffh

depend: FRC
	mkdep ${CFLAGS} ${HSRCS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 diff ${DESTDIR}/bin/diff
	install -s -o bin -g bin -m 755 diffh ${DESTDIR}/usr/lib/diffh

lint: FRC
	lint ${CFLAGS} ${SRCS}
	lint ${CFLAGS} ${HSRCS}

tags: FRC
	ctags ${SRCS}
	ctags ${HSRCS}
	sort -o tags tags

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

diffh.o: diffh.c /usr/include/stdio.h /usr/include/ctype.h
diffh.o: /usr/include/sys/types.h /usr/include/sys/stat.h
diff.o: diff.c diff.h /usr/include/stdio.h /usr/include/ctype.h
diff.o: /usr/include/sys/param.h /usr/include/sys/types.h /usr/include/signal.h
diff.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
diff.o: /usr/include/machine/endian.h /usr/include/sys/stat.h
diff.o: /usr/include/sys/dir.h /usr/include/signal.h
diffdir.o: diffdir.c diff.h /usr/include/stdio.h /usr/include/ctype.h
diffdir.o: /usr/include/sys/param.h /usr/include/sys/types.h
diffdir.o: /usr/include/signal.h /usr/include/machine/trap.h
diffdir.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
diffdir.o: /usr/include/sys/stat.h /usr/include/sys/dir.h /usr/include/signal.h
diffdir.o: /usr/include/a.out.h /usr/include/sys/exec.h
diffreg.o: diffreg.c diff.h /usr/include/stdio.h /usr/include/ctype.h
diffreg.o: /usr/include/sys/param.h /usr/include/sys/types.h
diffreg.o: /usr/include/signal.h /usr/include/machine/trap.h
diffreg.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
diffreg.o: /usr/include/sys/stat.h /usr/include/sys/dir.h /usr/include/signal.h
diffreg.o: /usr/include/a.out.h /usr/include/sys/exec.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
