#	Makefile	4.1	83/02/12

CFLAGS=-O -DNEQN -I../eqn/
YFLAGS=-d

SOURCE = e.y e.h diacrit.c eqnbox.c font.c fromto.c funny.c glob.c integral.c \
 io.c lex.c lookup.c mark.c matrix.c move.c over.c paren.c \
 pile.c shift.c size.c sqrt.c text.c

FILES = e.o diacrit.o eqnbox.o font.o fromto.o funny.o glob.o integral.o \
 io.o lex.o lookup.o mark.o matrix.o move.o over.o paren.o \
 pile.o shift.o size.o sqrt.o text.o

neqn:	$(FILES)
	cc $(CFLAGS) $(FILES) -o neqn

e.c:	e.def

e.def:	../eqn/e.y
	yacc -d ../eqn/e.y
	mv y.tab.c e.c
	mv y.tab.h e.def

$(FILES):	../eqn/e.h e.def
install: neqn
	install -s neqn $(DESTDIR)/usr/bin

clean:
	rm -f *.o e.c e.def

diacrit.o:	../eqn/diacrit.c
	$(CC) $(CFLAGS) -c ../eqn/diacrit.c

eqnbox.o:	../eqn/eqnbox.c
	$(CC) $(CFLAGS) -c ../eqn/eqnbox.c

font.o:	../eqn/font.c
	$(CC) $(CFLAGS) -c ../eqn/font.c

fromto.o:	../eqn/fromto.c
	$(CC) $(CFLAGS) -c ../eqn/fromto.c

funny.o:	../eqn/funny.c
	$(CC) $(CFLAGS) -c ../eqn/funny.c

glob.o:	../eqn/glob.c
	$(CC) $(CFLAGS) -c ../eqn/glob.c

integral.o:	../eqn/integral.c
	$(CC) $(CFLAGS) -c ../eqn/integral.c

io.o:	../eqn/io.c
	$(CC) $(CFLAGS) -c ../eqn/io.c

lex.o:	../eqn/lex.c
	$(CC) $(CFLAGS) -c ../eqn/lex.c

lookup.o:	../eqn/lookup.c
	$(CC) $(CFLAGS) -c ../eqn/lookup.c

mark.o:	../eqn/mark.c
	$(CC) $(CFLAGS) -c ../eqn/mark.c

matrix.o:	../eqn/matrix.c
	$(CC) $(CFLAGS) -c ../eqn/matrix.c

move.o:	../eqn/move.c
	$(CC) $(CFLAGS) -c ../eqn/move.c

over.o:	../eqn/over.c
	$(CC) $(CFLAGS) -c ../eqn/over.c

paren.o:	../eqn/paren.c
	$(CC) $(CFLAGS) -c ../eqn/paren.c

pile.o:	../eqn/pile.c
	$(CC) $(CFLAGS) -c ../eqn/pile.c

shift.o:	../eqn/shift.c
	$(CC) $(CFLAGS) -c ../eqn/shift.c

size.o:	../eqn/size.c
	$(CC) $(CFLAGS) -c ../eqn/size.c

sqrt.o:	../eqn/sqrt.c
	$(CC) $(CFLAGS) -c ../eqn/sqrt.c

text.o:	../eqn/text.c
	$(CC) $(CFLAGS) -c ../eqn/text.c
