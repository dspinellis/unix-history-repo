#	@(#)Makefile	5.2 (Berkeley) %G%

PROG=	sc

#DOBACKUPS		backup copy of source files on save
#IEEE_MATH		need setsticky() calls in your signal handlers
#INTERNATIONAL		8-bit characters
#SIGVOID		signal routines are type void
#SIMPLE			no arrow keys, or lex.c blows up
#DFLT_PAGER=\"more\"

CFLAGS+=-I. -I${.CURDIR} -DBSD43 -DDFLT_PAGER=\"more\" -DSIGVOID
SRCS=	cmds.c crypt.c gram.c help.c interp.c lex.c range.c sc.c \
	version.c vi.c vmtbl.c xmalloc.c
OBJS+=	gram.o
DPADD=	${LIBM} ${LIBCURSES} ${LIBTERMCAP}
LDADD=	-lm -lcurses -ltermcap
CLEANFILES+=experres.h statres.h y.tab.h gram.c

experres.h: gram.y
	sed -f ${.CURDIR}/eres.sed < ${.CURDIR}/gram.y > experres.h

statres.h: gram.y
	sed -f ${.CURDIR}/sres.sed < ${.CURDIR}/gram.y > statres.h

.depend: experres.h statres.h

.include <bsd.prog.mk>
