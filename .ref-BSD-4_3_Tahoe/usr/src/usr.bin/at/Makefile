#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.6	(Berkeley)	6/18/87
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	atq.c atrm.c at.c atrun.c
OBJS=	atq.o atrm.o at.o atrun.o
ATRUNDIR=	/usr/lib
SPOOLDIRS=	/usr/spool/at /usr/spool/at/past
SPOOLFILES=	/usr/spool/at/lasttimedone

all: atq atrm at atrun

at: at.c ${LIBC}
	${CC} ${CFLAGS} -o $@ at.c

atq: atq.c ${LIBC}
	${CC} ${CFLAGS} -o $@ atq.c

atrm: atrm.c ${LIBC}
	${CC} ${CFLAGS} -o $@ atrm.c

atrun: atrun.c ${LIBC}
	${CC} ${CFLAGS} -o $@ atrun.c

clean: FRC
	rm -f ${OBJS} core atq atrm at atrun

depend: FRC
	mkdep -p ${CFLAGS} ${SRCS}

install: FRC
	-for i in ${SPOOLDIRS}; do \
		mkdir ${DESTDIR}/$$i > /dev/null 2>&1; \
		chown root ${DESTDIR}/$$i; \
		chgrp daemon ${DESTDIR}/$$i; \
		chmod 755 ${DESTDIR}/$$i; \
	done
	-for i in ${SPOOLFILES}; do \
		if [ ! -f ${DESTDIR}/$$i ]; then \
			echo 0 > ${DESTDIR}/$$i; \
		fi; \
		chown root ${DESTDIR}/$$i; \
		chgrp daemon ${DESTDIR}/$$i; \
	done
	install -s -o bin -g bin -m 755 atrun ${DESTDIR}/${ATRUNDIR}
	install -s -o root -g bin -m 4755 atq ${DESTDIR}/usr/bin
	install -s -o root -g bin -m 4755 at ${DESTDIR}/usr/bin
	install -s -o root -g bin -m 4755 atrm ${DESTDIR}/usr/bin

lint: FRC
	lint ${CFLAGS} atq.c
	lint ${CFLAGS} atrm.c
	lint ${CFLAGS} at.c
	lint ${CFLAGS} atrun.c

tags: FRC
	ctags atq.c
	ctags -a atrm.c
	ctags -a at.c
	ctags -a atrun.c
	sort -o tags tags

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

atq: atq.c /usr/include/stdio.h /usr/include/sys/types.h
atq: /usr/include/sys/file.h /usr/include/sys/dir.h /usr/include/sys/stat.h
atq: /usr/include/sys/time.h /usr/include/time.h /usr/include/pwd.h
atq: /usr/include/ctype.h
atrm: atrm.c /usr/include/stdio.h /usr/include/pwd.h /usr/include/ctype.h
atrm: /usr/include/sys/types.h /usr/include/sys/dir.h /usr/include/sys/file.h
atrm: /usr/include/sys/stat.h
at: at.c /usr/include/stdio.h /usr/include/ctype.h /usr/include/signal.h
at: /usr/include/machine/trap.h /usr/include/pwd.h /usr/include/sys/param.h
at: /usr/include/sys/types.h /usr/include/signal.h
at: /usr/include/machine/machparam.h /usr/include/machine/endian.h
at: /usr/include/sys/time.h /usr/include/time.h /usr/include/sys/file.h
atrun: atrun.c /usr/include/stdio.h /usr/include/sys/param.h
atrun: /usr/include/sys/types.h /usr/include/signal.h
atrun: /usr/include/machine/trap.h /usr/include/machine/machparam.h
atrun: /usr/include/machine/endian.h /usr/include/sys/dir.h
atrun: /usr/include/sys/file.h /usr/include/sys/time.h /usr/include/time.h
atrun: /usr/include/sys/stat.h /usr/include/pwd.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
