#	@(#)Makefile	5.6 (Berkeley) 6/17/90

PROG=	f77pass1
CFLAGS+=-DHERE=VAX -DTARGET=VAX -DFAMILY=PCC -DUCBVAXASM -DUCBPASS2 \
	-DNAMELIST -I. -I${.CURDIR} -I${.CURDIR}/../include.vax
SRCS=	bb.c conv.c data.c equiv.c error.c exec.c expr.c fmt.c init.c \
	intr.c io.c lex.c main.c misc.c optcse.c optim.c optloop.c paramset.c \
	proc.c put.c putpcc.c regalloc.c setfmt.c stab.c vax.c vaxx.c
OBJS+=	gram.o
BINDIR=	/usr/libexec
NOMAN=	noman
CLEANFILES+=gram.c gram.in y.tab.c tokdefs.h

gram.c: gram.head gram.dcl gram.expr gram.exec gram.io tokdefs.h
	(sed < tokdefs.h "s/#define/%token/"; cd ${.CURDIR}; \
	    cat gram.head gram.dcl gram.expr gram.exec gram.io) > gram.in
	${YACC} gram.in
	mv y.tab.c gram.c

tokdefs.h: ${.CURDIR}/tokens
	(cd ${.CURDIR}; grep -n . < tokens | \
	    sed "s/\([^:]*\):\(.*\)/#define \2 \1/") > tokdefs.h

.include <bsd.prog.mk>

.depend: tokdefs.h
