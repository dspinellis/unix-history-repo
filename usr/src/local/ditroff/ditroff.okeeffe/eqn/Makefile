
#
#	Makefile	(CWI)	1.1	85/03/01
#
CFLAGS= -O
YFLAGS= -d
DESTDIR=
BINDIR=/usr/local
OBJECTS= diacrit.o eqnbox.o font.o fromto.o funny.o glob.o integral.o \
 io.o lex.o lookup.o mark.o matrix.o move.o over.o paren.o \
 pile.o shift.o size.o sqrt.o text.o e.o

all:	dieqn

dieqn:	${OBJECTS}
	cc -n -o dieqn ${CFLAGS} ${OBJECTS}

y.tab.h:	e.o

e.def:	y.tab.h
	-cmp -s y.tab.h e.def || cp y.tab.h e.def

${OBJECTS}:	e.h e.def

install: all
	install -s dieqn ${DESTDIR}${BINNDIR}/dieqn
	install -c -m 644 dieqn.1 ${DESTDIR}/usr/man/manl/dieqn.1

clean:
	rm -f dieqn *.o make.out y.tab.h e.def
