#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	1.3 (Berkeley) 6/16/87
#
DESTDIR=
CFLAGS=-O -I.
LIBC=	/lib/libc.a
SRCS=	C.c ctags.c fortran.c lisp.c print.c tree.c yacc.c
OBJS=	C.o ctags.o fortran.o lisp.o print.o tree.o yacc.o

all: ctags

ctags: ${OBJS} ${LIBC}
	${CC} ${CFLAGS} -o $@ ${OBJS}

install: FRC
	install -s -o bin -g bin -m 755 ctags ${DESTDIR}/usr/ucb/ctags

clean: FRC
	rm -f ${OBJS} core ctags

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

C.o: C.c /usr/include/stdio.h ctags.h /usr/include/stdio.h /usr/include/ctype.h
ctags.o: ctags.c ctags.h /usr/include/stdio.h /usr/include/ctype.h
ctags.o: /usr/include/strings.h
fortran.o: fortran.c ctags.h /usr/include/stdio.h /usr/include/ctype.h
fortran.o: /usr/include/strings.h
lisp.o: lisp.c ctags.h /usr/include/stdio.h /usr/include/ctype.h
lisp.o: /usr/include/strings.h
print.o: print.c /usr/include/sys/types.h /usr/include/sys/file.h ctags.h
print.o: /usr/include/stdio.h /usr/include/ctype.h
tree.o: tree.c ctags.h /usr/include/stdio.h /usr/include/ctype.h
tree.o: /usr/include/strings.h
yacc.o: yacc.c ctags.h /usr/include/stdio.h /usr/include/ctype.h
yacc.o: /usr/include/strings.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
