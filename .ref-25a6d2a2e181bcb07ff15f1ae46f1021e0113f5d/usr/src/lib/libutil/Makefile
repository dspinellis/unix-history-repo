#
# Copyright (c) 1988 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation, advertising
# materials, and other materials related to such redistribution and
# use acknowledge that the software was developed by the University
# of California, Berkeley.  The name of the University may not be
# used to endorse or promote products derived from this software
# without specific prior written permission.  THIS SOFTWARE IS PROVIDED
# ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
# WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF MERCHANTIBILITY AND
# FITNESS FOR A PARTICULAR PURPOSE.
#
# @(#)Makefile	5.3 (Berkeley) %G%
#

DEFS=	-DLIBC_SCCS
CFLAGS=	-O ${DEFS}
SRCS=	login.c logout.c logwtmp.c
OBJS=	login.o logout.o logwtmp.o

.c.o:
	@${CC} -p ${CFLAGS} -c $*.c
	@-ld -X -o profiled/$*.o -r $*.o
	${CC} ${CFLAGS} -c $*.c
	@-ld -x -r $*.o
	@mv a.out $*.o

all: libutil.a libutil_p.a llib-lutil.ln

libutil.a libutil_p.a: ${OBJS}
	@echo building normal libutil
	@ar cu libutil.a ${OBJS}
	ranlib libutil.a
	@echo building profiled libutil
	@cd profiled; ar cu ../libutil_p.a ${OBJS}
	ranlib libutil_p.a

llib-lutil.ln: ${SRCS}
	lint -Cutil ${CFLAGS} ${SRCS}

clean:
	rm -f ${OBJS} profiled/*.o libutil.a libutil_p.a

cleandir: clean
	rm -f .depend

depend:
	mkdep ${CFLAGS} ${SRCS}

install:
	install -o bin -g bin -m 644 libutil.a ${DESTDIR}/usr/lib
	ranlib -t ${DESTDIR}/usr/lib/libutil.a
	install -o bin -g bin -m 644 libutil_p.a ${DESTDIR}/usr/lib
	ranlib -t ${DESTDIR}/usr/lib/libutil_p.a
	install -o bin -g bin -m 644 llib-lutil.ln ${DESTDIR}/usr/libexec/lint

tags:
	ctags ${SRCS}
