unix=		We run UNIX.
SHELL=		/bin/sh

.SUFFIXES: .out .a .ln .o .c .F .f .e .r .y .l .s .cl .p .h \
		.8 .7 .6 .5 .4 .3 .2 .1 .0

# for RCS
#.SUFFIXES: .out .a .ln .o .c .F .f .e .r .y .l .s .cl .p .h \
#		.c,v .y,v .l,v .s,v .h,v .8 .7 .6 .5 .4 .3 .2 .1 .0

.LIBS:		.a
#.NULL:		.out

AR=		/usr/bin/ar
ARFLAGS=	rl

AS=		/usr/bin/as
AFLAGS=

CC=		/usr/bin/cc
CFLAGS=		-O

CI=		/usr/new/ci
CIFLAGS=
CO=		/usr/new/co
COFLAGS=

CPP=		/usr/bin/newcpp

FC=		/usr/bin/f77
FFLAGS=
EFLAGS=

LEX=		/usr/bin/lex
LFLAGS=

LINT=		/usr/bin/lint
LINTFLAGS=	-chapbx

MAKE=		/usr/src/usr.bin/pmake/obj/pmake

NROFF=		/usr/bin/nroff

PC=		/usr/bin/pc
PFLAGS=

RC=		/usr/bin/f77
RFLAGS=

YACC=		/usr/bin/yacc
YFLAGS=

#.c,v.c .y,v.y .l,v.l .s,v.s .h,v.h:
#	${CO} ${COFLAGS} ${.IMPSRC} ${.TARGET}

.c.o:
	${CC} ${CFLAGS} -c ${.IMPSRC}

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
