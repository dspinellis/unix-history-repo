#	@(#)Makefile	5.6 (Berkeley) %G%

PROG=	neqn
CFLAGS+=-DNEQN -I. -I${.CURDIR}/../common_source
YFLAGS=	-d
SRCS=	diacrit.c eqnbox.c font.c fromto.c funny.c glob.c integral.c io.c \
	lex.c lookup.c mark.c matrix.c move.c over.c paren.c pile.c shift.c \
	size.c sqrt.c text.c
OBJS+=	e.o
NOMAN=	noman
.PATH:	${.CURDIR}/../common_source
CLEANFILES+=e.c e.def y.tab.h

e.c:
	${YACC} ${YFLAGS} ${.CURDIR}/../common_source/e.y
	mv y.tab.c e.c
	mv y.tab.h e.def

.include "../../Makefile.inc"
.include <bsd.prog.mk>
