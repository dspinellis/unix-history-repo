#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.6	(Berkeley)	%G%
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

indent.o: indent.c
indent.o:indent_globs.h
indent.o: /usr/include/stdio.h
indent.o:indent_codes.h
io.o: io.c
io.o:indent_globs.h
io.o: /usr/include/stdio.h
lexi.o: lexi.c
lexi.o:indent_globs.h
lexi.o: /usr/include/stdio.h
lexi.o:indent_codes.h
lexi.o: /usr/include/ctype.h
parse.o: parse.c
parse.o:./indent_globs.h
parse.o: /usr/include/stdio.h
parse.o:./indent_codes.h
pr_comment.o: pr_comment.c
pr_comment.o:indent_globs.h
pr_comment.o: /usr/include/stdio.h
args.o: args.c
args.o:indent_globs.h
args.o: /usr/include/stdio.h /usr/include/sys/types.h /usr/include/ctype.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
