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
#	@(#)Makefile	4.9 (Berkeley) 10/10/88
#
CFLAGS=	-O
RES=
LIBC=	/lib/libc.a
SRCS=	nsquery.c nstest.c
OBJS=

all: nsquery nstest nslookup

nsquery: ${LIBC}
	${CC} ${CFLAGS} -o $@ nsquery.c ${RES}

# test programs and utilities
nstest: ${LIBC}
	@echo "The resolver routines in libc must be compiled with the -DDEBUG"
	@echo "option or nstest will not have any output."
	${CC} ${CFLAGS} -o $@ nstest.c ${RES}

nslookup: FRC
	cd nslookup; make ${MFLAGS} RES=${RES}

clean: FRC
	cd nslookup; make ${MFLAGS} clean
	rm -f ${OBJS} core nsquery nstest

cleandir: clean
	cd nslookup; make ${MFLAGS} cleandir
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	cd nslookup; make ${MFLAGS} depend
	mkdep -p ${CFLAGS} ${SRCS}

install: ${MAN}
	cd nslookup; make ${MFLAGS} DESTDIR=${DESTDIR} install

lint: ${SRCS}
	cd nslookup; make ${MFLAGS} lint
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	cd nslookup; make ${MFLAGS} tags
	ctags ${SRCS}

FRC:
