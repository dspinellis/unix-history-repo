#
# Copyright (c) 1987 Regents of the University of California.
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
#	@(#)Makefile	5.5 (Berkeley) %G%
#
CFLAGS=	-O -DDEBUG
LIBC=	/lib/libc.a
SRCS=	errormain.c errorinput.c errorpi.c errorsubr.c errorfilter.c \
	errortouch.c
OBJS=	errormain.o errorinput.o errorpi.o errorsubr.o errorfilter.o \
	errortouch.o

all: error

error:	${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS}

clean: FRC
	rm -f ${OBJS} core error

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 error ${DESTDIR}/usr/ucb/error

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

errormain.o: errormain.c /usr/include/stdio.h /usr/include/ctype.h
errormain.o: /usr/include/signal.h
errormain.o:error.h
errorinput.o: errorinput.c /usr/include/stdio.h /usr/include/ctype.h
errorinput.o:error.h
errorpi.o: errorpi.c /usr/include/stdio.h /usr/include/ctype.h
errorpi.o:error.h
errorsubr.o: errorsubr.c /usr/include/stdio.h /usr/include/ctype.h
errorsubr.o:error.h
errorfilter.o: errorfilter.c /usr/include/stdio.h /usr/include/ctype.h
errorfilter.o: /usr/include/pwd.h
errorfilter.o:error.h
errortouch.o: errortouch.c /usr/include/stdio.h /usr/include/ctype.h
errortouch.o: /usr/include/sys/types.h /usr/include/sys/stat.h
errortouch.o: /usr/include/signal.h
errortouch.o:error.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
