#
#	Makefile	(CWI)	2.2	87/04/01
#
CFLAGS= -O
YFLAGS= -d
DESTDIR=
BINDIR=/usr/local
OBJECTS= diacrit.o eqnbox.o font.o fromto.o funny.o glob.o input.o \
 integral.o lex.o lookup.o main.o mark.o matrix.o move.o over.o paren.o \
 pile.o shift.o size.o sqrt.o text.o eqn.o tuning.o

all:	dieqn ${FILES}

dieqn:	${OBJECTS}
	cc -o dieqn ${CFLAGS} ${OBJECTS}

y.tab.h:	eqn.o

prevy.tab.h:	y.tab.h
	-cmp -s y.tab.h prevy.tab.h || cp y.tab.h prevy.tab.h

${OBJECTS}:	e.h prevy.tab.h

install: all
	install -s dieqn ${DESTDIR}${BINDIR}/dieqn
	install -c -m 644 dieqn.1 ${DESTDIR}/usr/man/manl/dieqn.1

clean:
	rm -f dieqn *.o make.out y.tab.h prevy.tab.h
