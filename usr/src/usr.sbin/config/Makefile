#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.5	(Berkeley)	%G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
LIBS=	-ll
SRCS=	y.tab.c main.c lex.yy.c mkioconf.c mkmakefile.c \
	mkglue.c mkheaders.c mkswapconf.c
OBJS=	y.tab.o main.o lex.yy.o mkioconf.o mkmakefile.o \
	mkglue.o mkheaders.o mkswapconf.o

all: config

config: ${OBJS} ${LIBC}
	${CC} -o $@ ${OBJS} ${LIBS}

lex.yy.c: config.l
	lex config.l

y.tab.h: 
	yacc -d config.y

y.tab.c: config.y
	yacc config.y

clean: FRC
	rm -f ${OBJS} lex.yy.c y.tab.c y.tab.h config core

depend: y.tab.c lex.yy.c y.tab.h FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 config ${DESTDIR}/etc/config

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

y.tab.o: y.tab.c config.h /usr/include/sys/types.h /usr/include/ctype.h
y.tab.o: /usr/include/stdio.h
main.o: main.c /usr/include/stdio.h /usr/include/ctype.h y.tab.h config.h
main.o: /usr/include/sys/types.h
lex.yy.o: lex.yy.c /usr/include/stdio.h /usr/include/ctype.h y.tab.h config.h
lex.yy.o: /usr/include/sys/types.h
mkioconf.o: mkioconf.c /usr/include/stdio.h y.tab.h config.h
mkioconf.o: /usr/include/sys/types.h
mkmakefile.o: mkmakefile.c /usr/include/stdio.h /usr/include/ctype.h y.tab.h
mkmakefile.o: config.h /usr/include/sys/types.h
mkglue.o: mkglue.c /usr/include/stdio.h config.h /usr/include/sys/types.h
mkglue.o: y.tab.h /usr/include/ctype.h
mkheaders.o: mkheaders.c /usr/include/stdio.h /usr/include/ctype.h config.h
mkheaders.o: /usr/include/sys/types.h y.tab.h
mkswapconf.o: mkswapconf.c config.h /usr/include/sys/types.h
mkswapconf.o: /usr/include/stdio.h /usr/include/ctype.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
