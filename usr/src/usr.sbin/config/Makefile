#
#	Makefile	1.8	82/12/09
#
CFILES=	y.tab.c main.c lex.yy.c mkioconf.c mkmakefile.c mkubglue.c mkheaders.c
OBJS=	y.tab.o main.o lex.yy.o mkioconf.o mkmakefile.o mkubglue.o mkheaders.o

CFLAGS=-O
LDFLAGS=-ll

config: ${OBJS}
	${CC} ${OBJS} -o config ${LDFLAGS}

lex.yy.c: config.l
	lex config.l

y.tab.o: y.tab.c

y.tab.c: config.y
	yacc -d config.y

clean:
	rm -f ${OBJS} lex.yy.c y.tab.c y.tab.h config

install:
	install config ${DESTDIR}/etc/config

lint: ${CFILES}
	lint ${CFILES}
