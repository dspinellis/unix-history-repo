#
#	Makefile	1.6	82/10/24
#
CFILES=	y.tab.c main.c lex.yy.c mkioconf.c mkmakefile.c mkubglue.c mkheaders.c
OFILES=	y.tab.o main.o lex.yy.o mkioconf.o mkmakefile.o mkubglue.o mkheaders.o

CFLAGS=	-w
LDFLAGS=-ll

config: ${OFILES}
	${CC} ${OFILES} -o config ${LDFLAGS}

y.tab.o: y.tab.c

y.tab.c: config.y
	yacc -d config.y

lex.yy.c: config.l
	lex config.l

clean:
	rm -f ${OBJS} lex.yy.c y.tab.c y.tab.h config

install:
	install config ${DESTDIR}/etc/config

lint:
	lint ${CFILES}
