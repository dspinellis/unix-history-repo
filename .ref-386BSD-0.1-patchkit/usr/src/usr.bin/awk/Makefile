
PROG=	awk
YACC=	yacc -d
CFLAGS+= -DMAWK -I. -I${.CURDIR} -I${.CURDIR}/rexp 
SRCS=	parse.c scan.c memory.c main.c hash.c execute.c code.c \
	da.c error.c init.c bi_vars.c cast.c print.c bi_funct.c \
	kw.c jmp.c array.c field.c  split.c re_cmpl.c zmalloc.c \
	fin.c files.c  scancode.c matherr.c  fcall.c version.c
SRCS+=	rexp.c rexp0.c rexp1.c rexp2.c rexp3.c rexpdb.c

LDADD=	-lm -ly
DPADD=	${LIBMATH}
CLEANFILES+=parse.c parse.h
.PATH: ${.CURDIR}/rexp

parse.h parse.c:
	${YACC} -d ${.IMPSRC}
	mv y.tab.c parse.c
	cat ${.CURDIR}/parse2.xc >> parse.c
	mv y.tab.h parse.h


.include <bsd.prog.mk>
