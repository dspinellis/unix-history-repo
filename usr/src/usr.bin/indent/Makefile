#	@(#)Makefile	4.2	(Berkeley)	%G%
#
DESTDIR=
CFLAGS=	-O
OBJS=	indent.o io.o lexi.o parse.o pr_comment.o

indent: indent.o io.o lexi.o parse.o pr_comment.o
	cc -o indent ${OBJS}

clean:
	rm -f ${OBJS} indent

install:
	install -s indent ${DESTDIR}/usr/ucb/indent
