CFLAGS=-O
YFLAGS=-d
PREFIX = SCCS/s.

SOURCE = e.y e.h diacrit.c eqnbox.c font.c fromto.c funny.c glob.c integral.c \
 io.c lex.c lookup.c mark.c matrix.c move.c over.c paren.c \
 pile.c shift.c size.c sqrt.c text.c

FILES =  diacrit.o eqnbox.o font.o fromto.o funny.o glob.o integral.o \
 io.o lex.o lookup.o mark.o matrix.o move.o over.o paren.o \
 pile.o shift.o size.o sqrt.o text.o e.o

a.out:	$(FILES)
	cc -n -O $(CFLAGS) $(FILES) 

y.tab.h:	e.o

e.def:	y.tab.h
	-cmp -s y.tab.h e.def || cp y.tab.h e.def

$(FILES):	e.h e.def

list:
	@pr TODO $(SOURCE) makefile

install:
	strip a.out
	cp a.out /usr/bin/eqn

lint:	y.tab.c
	lint -spb *.c

clean:
	rm -f a.out *.o make.out y.tab.h e.def
