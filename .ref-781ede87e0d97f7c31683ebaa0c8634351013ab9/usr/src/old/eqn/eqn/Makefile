#	@(#)Makefile	5.6 (Berkeley) %G%

PROG=	eqn
CFLAGS+=-I. -I${.CURDIR}/../common_source
SRCS=	diacrit.c eqnbox.c font.c fromto.c funny.c glob.c integral.c io.c \
	lex.c lookup.c mark.c matrix.c move.c over.c paren.c pile.c shift.c \
	size.c sqrt.c text.c
OBJS+=	e.o
.PATH:	${.CURDIR}/../common_source
YFLAGS+=-d
MLINKS=	eqn.1 checkeq.1 eqn.1 neqn.1
CLEANFILES+=e.c e.def y.tab.h

e.c:
	${YACC} ${YFLAGS} ${.CURDIR}/../common_source/e.y
	mv y.tab.c e.c
	mv y.tab.h e.def

eqn.0:
	eqn ${.CURDIR}/eqn.1 | nroff -h -man > ${.TARGET}

.include "../../Makefile.inc"
.include <bsd.prog.mk>
