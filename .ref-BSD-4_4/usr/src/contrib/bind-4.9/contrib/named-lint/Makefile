# debugging isn't needed now (hah :-)
CFLAGS= -O
#CFLAGS= -g
# gcc seems better than cc
CC=gcc

BISON=bison
# bison: -v product .output file
BFLAGS=-v
LFLAGS=-v
LDLIBS=

.SUFFIXES:	.lex

.y.c:
	${BISON} ${BFLAGS} $< -o $*.c

.y.o:
	${BISON} ${BFLAGS} $< -o $*.c
	${COMPILE.c} $*.c

.lex.c:
	${LEX.l} $< > $*.c

.lex.o:
	${LEX.l} $< > $*.c
	${COMPILE.c} $*.c

named-lint:	named-lint.c
	${LINK.c} -o named-lint named-lint.c ${LDLIBS}

clean:
	rm -f named-lint named-lint.o named-lint.c named-lint.output *.BAK
