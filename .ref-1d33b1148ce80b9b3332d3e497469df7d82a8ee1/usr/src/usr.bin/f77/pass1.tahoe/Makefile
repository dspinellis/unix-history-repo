#	@(#)Makefile	5.3 (Berkeley) %G%

PROG=	f77pass1
CFLAGS+=-DHERE=TAHOE -DTARGET=TAHOE -DFAMILY=PCC -DUCBVAXASM -DUCBPASS2 \
	-I. -I${.CURDIR} -I${.CURDIR}/../include.tahoe
SRCS=	bb.c conv.c data.c equiv.c error.c exec.c expr.c fmt.c init.c \
	intr.c io.c lex.c main.c misc.c optcse.c optim.c optloop.c paramset.c \
	proc.c put.c putpcc.c regalloc.c setfmt.c stab.c tahoe.c tahoex.c
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
