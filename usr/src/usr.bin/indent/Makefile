#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.3 (Berkeley) %G%
#
DESTDIR=
CFLAGS=	-O
OBJS=	indent.o io.o lexi.o parse.o pr_comment.o

indent: indent.o io.o lexi.o parse.o pr_comment.o
	cc ${CFLAGS} -o indent ${OBJS}

clean:
	rm -f ${OBJS} indent

indent.o: indent.c
io.o: io.c
lexi.o: lexi.c
parse.o: parse.c
pr_comment.o: pr_comment.c
$(OBJS): indent_globs.h indent_codes.h

.DEFAULT:
	sccs get $<

install: indent
	install -s indent ${DESTDIR}/usr/ucb/indent
