CFLAGS = -O -w
LDFLAGS=

utran:          utran.o parse.o lex.o code.o sym.o mem.o \
                err.o char.o keyword.o optab.o toktab.o
	cc $(LDFLAGS) -o utran *.o

utran.o:        utran.h token.h tree.h sym.h
parse.o:        utran.h tree.h sym.h
lex.o:	        utran.h token.h lex.h char.h tree.h
code.o:         utran.h token.h tree.h code.h sym.h
sym.o:	        utran.h token.h sym.h char.h
mem.o:	        utran.h sym.h
err.o:	        utran.h token.h tree.h lex.h err.h
char.o:         char.h
keyword.o:      keyword.h sym.h
optab.o:        lex.h
toktab.o:       utran.h lex.h token.h

parse.c token.h: ucon.g
	yacc -v -d ucon.g; : expect 168 shift/reduce conflicts
	mv y.tab.c parse.c
	ed parse.c <pscript
	mv y.tab.h token.h

keyword.h keyword.c: keywords
	mkkeytab

toktab.c optab.c: tokens optab
	mktoktab

mkkeytab: mkkeytab.icn
	iconc -o mkkeytab mkkeytab.icn

mktoktab: mktoktab.icn
	iconc -o mktoktab mktoktab.icn

Listall:
	@pr *.h *.c ucon.g
	@date >List

List:		char.h code.h err.h keyword.h lex.h sym.h \
                token.h tree.h utran.h \
	        char.c code.c err.c keyword.c lex.c mem.c \
	        optab.c parse.c sym.c toktab.c utran.c ucon.g
	@pr $?
	@date >List
clean:
	rm -f *.o utran

dist-clean: clean
	rm -f `gcomp Makefile *.c *.h mkkeytab.icn mktoktab.icn tokens optab keywords ucon.g pscript *.gen`
