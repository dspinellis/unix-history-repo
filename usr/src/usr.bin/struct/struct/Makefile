#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.5	(Berkeley)	%G%
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

all: beautify structure

beautify: ${BOBJS} ${LIBC}
	${CC} ${CFLAGS} ${BOBJS} -o $@ -lln

structure: ${SOBJS} ${LIBC}
	${CC} ${CFLAGS} ${SOBJS} -o $@

clean: FRC
	rm -f ${OBJS} core beauty.c structure beautify y.tab.h lextab.c

depend: ${SRCS} FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 structure $(DESTDIR)/usr/lib/struct/structure
	install -s -o bin -g bin -m 755 beautify $(DESTDIR)/usr/lib/struct/beautify
	install -c -o bin -g bin -m 755 struct $(DESTDIR)/usr/bin/struct

lint: ${SRCS} FRC
	lint ${CFLAGS} ${BSRCS}
	lint ${CFLAGS} ${SSRCS}

tags: ${SRCS} FRC
	ctags ${BSRCS}
	ctags -a ${SSRCS}
	sort -o tags tags

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

beauty.o: beauty.c b.h /usr/include/stdio.h /usr/include/signal.h
tree.o: tree.c y.tab.h b.h /usr/include/stdio.h
lextab.o: lextab.c /usr/include/stdio.h y.tab.h b.h
bdef.o: bdef.c
0.alloc.o: 0.alloc.c /usr/include/stdio.h def.h
0.args.o: 0.args.c /usr/include/stdio.h def.h
0.def.o: 0.def.c /usr/include/stdio.h def.h
0.extr.o: 0.extr.c /usr/include/stdio.h def.h
0.graph.o: 0.graph.c /usr/include/stdio.h def.h
0.list.o: 0.list.c /usr/include/stdio.h def.h
0.parts.o: 0.parts.c /usr/include/stdio.h def.h
0.string.o: 0.string.c /usr/include/stdio.h def.h 1.defs.h
1.finish.o: 1.finish.c /usr/include/stdio.h def.h 1.incl.h
1.form.o: 1.form.c /usr/include/stdio.h 1.defs.h def.h
1.fort.o: 1.fort.c /usr/include/stdio.h 1.incl.h 1.defs.h def.h
1.hash.o: 1.hash.c /usr/include/stdio.h 1.incl.h 1.defs.h def.h
1.init.o: 1.init.c /usr/include/stdio.h 1.defs.h 1.incl.h def.h
1.line.o: 1.line.c /usr/include/stdio.h def.h
1.main.o: 1.main.c /usr/include/stdio.h def.h
1.node.o: 1.node.c /usr/include/stdio.h def.h 1.incl.h
1.recog.o: 1.recog.c /usr/include/stdio.h 1.incl.h def.h
1.tables.o: 1.tables.c /usr/include/stdio.h
2.dfs.o: 2.dfs.c /usr/include/stdio.h def.h 2.def.h
2.dom.o: 2.dom.c /usr/include/stdio.h def.h 2.def.h
2.head.o: 2.head.c /usr/include/stdio.h def.h 2.def.h
2.inarc.o: 2.inarc.c /usr/include/stdio.h def.h 2.def.h
2.main.o: 2.main.c /usr/include/stdio.h def.h 2.def.h
2.tree.o: 2.tree.c /usr/include/stdio.h def.h 2.def.h
3.branch.o: 3.branch.c /usr/include/stdio.h def.h 3.def.h
3.flow.o: 3.flow.c /usr/include/stdio.h def.h 3.def.h
3.loop.o: 3.loop.c /usr/include/stdio.h def.h 3.def.h
3.main.o: 3.main.c /usr/include/stdio.h def.h
3.reach.o: 3.reach.c /usr/include/stdio.h def.h
3.then.o: 3.then.c /usr/include/stdio.h def.h 3.def.h
4.brace.o: 4.brace.c /usr/include/stdio.h def.h 4.def.h 3.def.h
4.form.o: 4.form.c /usr/include/stdio.h def.h 4.def.h
4.main.o: 4.main.c /usr/include/stdio.h def.h 4.def.h
4.out.o: 4.out.c /usr/include/stdio.h def.h 4.def.h 3.def.h
main.o: main.c /usr/include/signal.h /usr/include/stdio.h 1.defs.h def.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
