#
#	Makefile	(CWI)	2.1	87/04/01
#
CFLAGS=
YFLAGS= -d
DESTDIR=
BINDIR=/usr/local
OBJECTS= diacrit.o eqnbox.o font.o fromto.o funny.o glob.o integral.o \
 input.o main.o lex.o lookup.o mark.o matrix.o move.o over.o paren.o \
 pile.o shift.o size.o sqrt.o text.o eqn.o

all:	dieqn ${FILES}

dieqn:	${OBJECTS}
	cc -o dieqn ${CFLAGS} ${OBJECTS}

y.tab.h:	eqn.o

e.def:	y.tab.h
	-cmp -s y.tab.h e.def || cp y.tab.h e.def

${OBJECTS}:	e.h e.def

install: all
	install -s dieqn ${DESTDIR}${BINDIR}/dieqn
	install -c -m 644 dieqn.1 ${DESTDIR}/usr/man/manl/dieqn.1

clean:
	rm -f dieqn *.o make.out y.tab.h e.def
