#
# Copyright {c} 1987 The Regents of the University of California.
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
#	@(#)Makefile	4.6 {Berkeley} %G%
#
CFLAGS=	-O
YFLAGS=	-d
LIBC=	/lib/libc.a
BSRCS=	beauty.c tree.c lextab.c bdef.c
BOBJS=	beauty.o tree.o lextab.o bdef.o
SSRCS=	0.alloc.c 0.args.c 0.def.c 0.extr.c 0.graph.c 0.list.c 0.parts.c \
	0.string.c 1.finish.c 1.form.c 1.fort.c 1.hash.c 1.init.c 1.line.c \
	1.main.c 1.node.c 1.recog.c 1.tables.c 2.dfs.c 2.dom.c 2.head.c \
	2.inarc.c 2.main.c 2.tree.c 3.branch.c 3.flow.c 3.loop.c 3.main.c \
	3.reach.c 3.then.c 4.brace.c 4.form.c 4.main.c 4.out.c main.c
SOBJS=	0.alloc.o 0.args.o 0.def.o 0.extr.o 0.graph.o 0.list.o 0.parts.o \
	0.string.o 1.finish.o 1.form.o 1.fort.o 1.hash.o 1.init.o 1.line.o \
	1.main.o 1.node.o 1.recog.o 1.tables.o 2.dfs.o 2.dom.o 2.head.o \
	2.inarc.o 2.main.o 2.tree.o 3.branch.o 3.flow.o 3.loop.o 3.main.o \
	3.reach.o 3.then.o 4.brace.o 4.form.o 4.main.o 4.out.o main.o
SRCS=	${BSRCS} ${SSRCS}
OBJS=	${BOBJS} ${SOBJS}
MAN=	struct.0

all: beautify structure

beautify: ${BOBJS} ${LIBC}
	${CC} ${CFLAGS} ${BOBJS} -o $@ -lln

structure: ${SOBJS} ${LIBC}
	${CC} ${CFLAGS} ${SOBJS} -o $@

clean:
	rm -f ${OBJS} core beauty.c structure beautify y.tab.h lextab.c

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep ${CFLAGS} ${SRCS}

install: ${MAN}
	-[ -d ${DESTDIR}/usr/lib/struct ] || mkdir ${DESTDIR}/usr/lib/struct
	install -s -o bin -g bin -m 755 structure ${DESTDIR}/usr/lib/struct
	install -s -o bin -g bin -m 755 beautify ${DESTDIR}/usr/lib/struct
	install -c -o bin -g bin -m 755 struct ${DESTDIR}/usr/bin/struct
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat1

lint: ${SRCS}
	lint ${CFLAGS} ${BSRCS}
	lint ${CFLAGS} ${SSRCS}

tags: ${SRCS}
	ctags ${BSRCS}
	ctags -a ${SSRCS}
	sort -o tags tags
