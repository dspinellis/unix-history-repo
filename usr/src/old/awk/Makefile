#	@(#)Makefile	4.12 (Berkeley) %G%

YFLAGS=	-d
CFLAGS+=-I. -I${.CURDIR}
PROG=	awk
SRCS=	awk.lx.c b.c lib.c main.c parse.c proctab.c freeze.c run.c tran.c
OBJS+=	token.o awk.g.o
CLEANFILES+=	token.c proctab.c proc.o proc awk.h awk.lx.c y.tab.h

DPADD=	${LIBM}
LDADD=	-lm

token.c: awk.h
	echo '#include "awk.h"' > ${.TARGET}
	echo 'struct toke { char *tnm; int yval; } tok[] = {' >> ${.TARGET}
	sed 's/#define \([^ ]*\) \(.*\)/"\1", \2,/' \
	    < awk.h >> ${.TARGET}
	echo '};' >> ${.TARGET}
	echo 'char *tokname(n)' >> ${.TARGET}
	echo '{' >> ${.TARGET}
	echo '	if (n <= 256 || n >= LASTTOKEN)' >> ${.TARGET}
	echo '		n = 257;' >> ${.TARGET}
	echo '	return(tok[n - 257].tnm);' >> ${.TARGET}
	echo '}' >> ${.TARGET}

awk.h: awk.g.o
	rm -f awk.h
	cp y.tab.h awk.h

proctab.c: proc
	./proc > proctab.c

proc: token.o proc.o
	${CC} -o ${.TARGET} proc.o token.o

.include <bsd.prog.mk>
