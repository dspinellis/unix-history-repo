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
#	@(#)Makefile	5.8 (Berkeley) 6/29/88
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	indent.c io.c lexi.c parse.c pr_comment.c args.c
OBJS=	indent.o io.o lexi.o parse.o pr_comment.o args.o

all: indent

indent:	${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS}

clean: FRC
	rm -f ${OBJS} core indent

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 indent ${DESTDIR}/usr/ucb/indent

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

indent.o: indent.c indent_globs.h /usr/include/stdio.h indent_codes.h
io.o: io.c indent_globs.h /usr/include/stdio.h
lexi.o: lexi.c indent_globs.h /usr/include/stdio.h indent_codes.h
lexi.o: /usr/include/ctype.h
parse.o: parse.c ./indent_globs.h /usr/include/stdio.h ./indent_codes.h
pr_comment.o: pr_comment.c indent_globs.h /usr/include/stdio.h
args.o: args.c indent_globs.h /usr/include/stdio.h /usr/include/sys/types.h
args.o: /usr/include/ctype.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
