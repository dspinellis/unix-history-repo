unix=		We run UNIX.
LIBC=		/lib/libc.a
SHELL=		/bin/sh

.MAIN: all

.SUFFIXES: .out .a .ln .o .c .F .f .e .r .y .l .s .cl .p .h \
		.c,v .y,v .l,v .s,v .h,v .8 .7 .6 .5 .4 .3 .2 .1 .0
.INCLUDES:	.h
.LIBS:		.a
.NULL:		.out

AR=		ar
ARFLAGS=	rl

AS=		as
AFLAGS=

CC=		cc
CFLAGS=		-O

CI=		ci
CIFLAGS=
CO=		co
COFLAGS=

CPP=		newcpp

FC=		f77
FFLAGS=
EFLAGS=

LEX=		lex
LFLAGS=

LINT=		lint
LINTFLAGS=	-chapbx

MAKE=		make

NROFF=		nroff

PC=		pc
PFLAGS=

RC=		f77
RFLAGS=

YACC=		yacc
YFLAGS=

.c,v.c .y,v.y .l,v.l .s,v.s .h,v.h:
	${CO} ${COFLAGS} ${.IMPSRC} ${.TARGET}

.c.o:
	${CC} ${.INCLUDES} ${CFLAGS} -c ${.IMPSRC}

.p.o:
	${PC} ${PFLAGS} -c ${.IMPSRC}

.cl.o:
	class -c ${.IMPSRC}

.e.o .r.o .F.o .f.o:
	${FC} ${RFLAGS} ${EFLAGS} ${FFLAGS} -c ${.IMPSRC}

.s.o:
	${AS} ${AFLAGS} -o ${.TARGET} ${.IMPSRC}

.y.o:
	${YACC} ${YFLAGS} ${.IMPSRC}
	${CC} ${CFLAGS} -c y.tab.c -o ${.TARGET}
	rm y.tab.c

.l.o:
	${LEX} ${LFLAGS} ${.IMPSRC}
	${CC} ${CFLAGS} -c lex.yy.c -o ${.TARGET}
	rm lex.yy.c

.y.c:
	${YACC} ${YFLAGS} ${.IMPSRC}
	mv y.tab.c ${.TARGET}

.l.c:
	${LEX} ${LFLAGS} ${.IMPSRC}
	mv lex.yy.c ${.TARGET}

.s.out .c.out .o.out:
	${CC} ${CFLAGS} ${.IMPSRC} ${LDLIBS} -o ${.TARGET}

.f.out .F.out .r.out .e.out:
	${FC} ${EFLAGS} ${RFLAGS} ${FFLAGS} ${.IMPSRC} \
	    ${LDLIBS} -o ${.TARGET}
	rm -f ${.PREFIX}.o

.y.out:
	${YACC} ${YFLAGS} ${.IMPSRC}
	${CC} ${CFLAGS} y.tab.c ${LDLIBS} -ly -o ${.TARGET}
	rm y.tab.c

.l.out:
	${LEX} ${LFLAGS} ${.IMPSRC}
	${CC} ${CFLAGS} lex.yy.c ${LDLIBS} -ll -o ${.TARGET}
	rm lex.yy.c

.8.0 .7.0 .6.0 .5.0 .4.0 .3.0 .2.0 .1.0:
	${NROFF} -man -h ${.IMPSRC} > ${.TARGET}

DEPENDFILE=	.depend
.if exists(${DEPENDFILE})
.include ".depend"
.endif


