#
# Copyright (c) 1988 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation,
# advertising materials, and other materials related to such
# distribution and use acknowledge that the software was developed
# by the University of California, Berkeley.  The name of the
# University may not be used to endorse or promote products derived
# from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	5.7 (Berkeley) %G%
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

depend: y.tab.h ${SRCS} FRC
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
main.o: main.c /usr/include/sys/types.h /usr/include/sys/stat.h
main.o: /usr/include/sys/file.h /usr/include/stdio.h /usr/include/ctype.h
main.o: y.tab.h config.h /usr/include/sys/types.h
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
