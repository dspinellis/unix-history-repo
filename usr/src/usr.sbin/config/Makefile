#
#	Makefile	1.7	82/10/25
#
CFILES=	y.tab.c main.c lex.yy.c mkioconf.c mkmakefile.c mkubglue.c mkheaders.c
OFILES=	y.tab.o main.o lex.yy.o mkioconf.o mkmakefile.o mkubglue.o mkheaders.o

CFLAGS=
LDFLAGS=-ll

config: ${OFILES}
	${CC} ${OFILES} -o config ${LDFLAGS}

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
