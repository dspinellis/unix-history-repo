#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	1.7	(Berkeley)	%G%
#
CFLAGS=	-O -Dunix=1 -DFLEXNAMES
LIBC=	/lib/libc.a
YYFIX=	../../libexec/pcc/pcc.${MACHINE}/:yyfix
SRCS=	cpp.c cpy.c rodata.c
OBJS=	cpp.o cpy.o rodata.o

all: cpp

cpp:	${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS}

cpy.c: cpy.y
	yacc cpy.y
	${YYFIX} yyexca yyact yypact yypgo yyr1 yyr2 yychk yydef
	mv y.tab.c cpy.c

cpy.o: cpy.c yylex.c

rodata.o: cpy.c
	${CC} ${CFLAGS} -R -c rodata.c

clean:
	rm -f ${OBJS} cpp core cpy.c rodata.c

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep ${CFLAGS} ${SRCS}

install: ${MAN}
	install -s -o bin -g bin -m 755 cpp ${DESTDIR}/usr/bin

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}
