#	@(#)Makefile	5.1 (Berkeley) %G%

PROG=	sh
SRCS=	builtins.c cd.c dirent.c echo.c error.c eval.c exec.c expand.c \
	input.c jobs.c mail.c main.c memalloc.c miscbltin.c \
	mystring.c nodes.c options.c parser.c redir.c show.c signames.c \
	syntax.c trap.c output.c var.c
OBJS+=	init.o
ORIG=	${.CURDIR}/../../../../bin/sh
CFLAGS+=-DLETS_GET_SMALL -DSHELL -I. -I${ORIG}
NOMAN=	noman
LDFLAGS=-N
.PATH:	${ORIG} ${ORIG}/bltin
CLEANFILES+=\
	builtins.c builtins.h init.c mkinit mknodes mksignames mksyntax \
	nodes.c nodes.h signames.c signames.h syntax.c syntax.h token.def

.depend parser.o: token.def
token.def: mktokens
	sh ${ORIG}/mktokens

builtins.h builtins.c: ${ORIG}/mkbuiltins ${ORIG}/builtins
	cd ${.CURDIR}; sh mkbuiltins

init.c: mkinit ${SRCS}
	./mkinit '${CC} -c ${CFLAGS} init.c' ${.ALLSRC}

mkinit: ${ORIG}/mkinit.c
	${CC} ${CFLAGS} ${ORIG}/mkinit.c -o $@

nodes.c nodes.h: mknodes ${ORIG}/nodetypes ${ORIG}/nodes.c.pat
	./mknodes ${ORIG}/nodetypes ${ORIG}/nodes.c.pat

mknodes: ${ORIG}/mknodes.c
	${CC} ${CFLAGS} ${ORIG}/mknodes.c -o $@

signames.c signames.h: mksignames
	./mksignames

mksignames: ${ORIG}/mksignames.c
	${CC} ${CFLAGS} ${ORIG}/mksignames.c -o $@

syntax.c syntax.h: mksyntax
	./mksyntax

mksyntax: ${ORIG}/mksyntax.c ${ORIG}/parser.h
	${CC} ${CFLAGS} ${ORIG}/mksyntax.c -o $@

.include <bsd.prog.mk>
