#
# Copyright (c) 1988 Regents of the University of California.
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
#	@(#)Makefile	5.22 (Berkeley) 1/6/89
#
DEFS=	-DLIBC_SCCS
CFLAGS=	-O ${DEFS}
SRCS=	getnetbyaddr.c getnetent.c getnetbyname.c getproto.c getprotoent.c \
	getprotoname.c getservent.c getservbyport.c getservbyname.c \
	herror.c rcmd.c rexec.c res_comp.c res_debug.c \
	res_init.c res_mkquery.c res_query.c res_send.c
OBJS=	getnetbyaddr.o getnetent.o getnetbyname.o getproto.o getprotoent.o \
	getprotoname.o getservent.o getservbyport.o getservbyname.o \
	herror.o rcmd.o rexec.o res_comp.o res_debug.o \
	res_init.o res_mkquery.o res_query.o res_send.o
TAGSFILE=tags

.c.o:
	@${CC} -p -c ${CFLAGS} $*.c
	@-ld -X -o profiled/$*.o -r $*.o
	${CC} ${CFLAGS} -c $*.c
	@-ld -x -r $*.o
	@mv a.out $*.o

all: ${OBJS}

link: ${OBJS}
	(cd ../library; rm -f ${OBJS})
	(cd ../profiled; rm -f ${OBJS})
	ln ${OBJS} ../library
	(cd profiled; ln ${OBJS} ../../profiled)

tags:
	cwd=`pwd`; \
	for i in ${SRCS}; do \
		ctags -a -f ${TAGSFILE} $$cwd/$$i; \
	done

clean:
	rm -f *.o profiled/* a.out core ${TAGSFILE}

cleandir: clean
	rm -f .depend

depend:
	mkdep ${CFLAGS} ${SRCS}
