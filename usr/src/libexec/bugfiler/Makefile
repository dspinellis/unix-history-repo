#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that this notice is preserved and that due credit is given
# to the University of California at Berkeley. The name of the University
# may not be used to endorse or promote products derived from this
# software without specific prior written permission. This software
# is provided ``as is'' without express or implied warranty.
#
#	@(#)Makefile	5.11 (Berkeley) %G%
#
CFLAGS=	-O -I.
LIBC=	/lib/libc.a
SRCS=	bugfiler.c error.c gethead.c process.c redist.c reply.c
OBJS=	bugfiler.o error.o gethead.o process.o redist.o reply.o

all: bugfiler

bugfiler: ${OBJS} ${LIBC}
	${CC} ${CFLAGS} -o $@ ${OBJS}

clean: FRC
	rm -f ${OBJS} core bugfiler

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o root -g bin -m 4755 bugfiler ${DESTDIR}/usr/lib/bugfiler
	install -c -o bin -g bin -m 755 sendbug.sh ${DESTDIR}/usr/ucb/sendbug
	install -c -o bin -g bin -m 644 bugformat ${DESTDIR}/usr/lib/bugformat

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

bugfiler.o: bugfiler.c
bugfiler.o:bug.h
bugfiler.o: /usr/include/sys/param.h /usr/include/sys/types.h
bugfiler.o: /usr/include/signal.h /usr/include/machine/machparam.h
bugfiler.o: /usr/include/sys/dir.h /usr/include/sys/time.h /usr/include/time.h
bugfiler.o: /usr/include/sys/file.h /usr/include/pwd.h /usr/include/stdio.h
error.o: error.c
error.o:bug.h
error.o: /usr/include/sys/param.h /usr/include/sys/types.h
error.o: /usr/include/signal.h /usr/include/machine/machparam.h
error.o: /usr/include/sys/dir.h /usr/include/syslog.h /usr/include/stdio.h
gethead.o: gethead.c
gethead.o:bug.h
gethead.o: /usr/include/sys/param.h /usr/include/sys/types.h
gethead.o: /usr/include/signal.h /usr/include/machine/machparam.h
gethead.o: /usr/include/sys/dir.h /usr/include/sys/stat.h /usr/include/stdio.h
process.o: process.c
process.o:bug.h
process.o: /usr/include/sys/param.h /usr/include/sys/types.h
process.o: /usr/include/signal.h /usr/include/machine/machparam.h
process.o: /usr/include/sys/dir.h /usr/include/sys/file.h
process.o: /usr/include/sys/time.h /usr/include/time.h /usr/include/stdio.h
process.o: /usr/include/ctype.h
redist.o: redist.c /usr/include/sys/file.h /usr/include/stdio.h
redist.o:bug.h
redist.o: /usr/include/sys/param.h /usr/include/sys/types.h
redist.o: /usr/include/signal.h /usr/include/machine/machparam.h
redist.o: /usr/include/sys/dir.h
reply.o: reply.c
reply.o:bug.h
reply.o: /usr/include/sys/param.h /usr/include/sys/types.h
reply.o: /usr/include/signal.h /usr/include/machine/machparam.h
reply.o: /usr/include/sys/dir.h /usr/include/sys/file.h /usr/include/stdio.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
