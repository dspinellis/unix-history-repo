#	@(#)Makefile	5.2 (Berkeley) %G%

PROG=	structure
SRCS=	0.alloc.c 0.args.c 0.def.c 0.extr.c 0.graph.c 0.list.c 0.parts.c \
	0.string.c 1.finish.c 1.form.c 1.fort.c 1.hash.c 1.init.c 1.line.c \
	1.main.c 1.node.c 1.recog.c 1.tables.c 2.dfs.c 2.dom.c 2.head.c \
	2.inarc.c 2.main.c 2.tree.c 3.branch.c 3.flow.c 3.loop.c 3.main.c \
	3.reach.c 3.then.c 4.brace.c 4.form.c 4.main.c 4.out.c main.c
MAN1=	struct.0
BINDIR=	/usr/libexec

beforeinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} \
	    ${.CURDIR}/struct.sh ${DESTDIR}/usr/bin/struct

.include <bsd.prog.mk>
