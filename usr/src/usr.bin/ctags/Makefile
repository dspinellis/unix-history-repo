#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.2 (Berkeley) %G%
#
DESTDIR=
CFLAGS=-O -I.
SRCS=	C.c ctags.c fortran.c lisp.c print.c tree.c yacc.c
OBJS=	C.o ctags.o fortran.o lisp.o print.o tree.o yacc.o

ctags: ${OBJS}
	${CC} ${CFLAGS} -o $@ ${OBJS}

install: ctags
	install -o root -g staff -m 755 -s ctags ${DESTDIR}/usr/ucb

clean:
	rm -f ctags *.o core

lint:
	lint -I. -chapbx ${SRCS}

tags:
	ctags -dt ${SRCS}

depend:

${OBJS}: ctags.h
