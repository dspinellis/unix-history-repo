#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	1.5	(Berkeley)	6/6/87
#
CFLAGS=	-O -Dunix=1 -DFLEXNAMES
LIBC=	/lib/libc.a
YYFIX=	../pcc/pcc.${MACHINE}/:yyfix
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

clean: FRC
	rm -f ${OBJS} cpp core cpy.c rodata.c

depend: ${SRCS} FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 cpp ${DESTDIR}/lib/cpp

lint: ${SRCS} FRC
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS} FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

cpp.o: cpp.c /usr/include/sys/param.h /usr/include/sys/types.h
cpp.o: /usr/include/signal.h /usr/include/machine/trap.h
cpp.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
cpp.o: /usr/include/stdio.h /usr/include/ctype.h
cpy.o: cpy.c yylex.c
rodata.o: rodata.c

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
