#	Makefile	4.2	87/05/31
#
CFLAGS = -O

all: lex

lex: lmain.o y.tab.o sub1.o sub2.o header.o
	cc lmain.o y.tab.o sub1.o sub2.o header.o -o lex

smallex:
	cc -DSMALL -O lmain.c y.tab.c sub1.c sub2.c header.c -o smallex

y.tab.c: parser.y
	yacc parser.y
lmain.o:lmain.c ldefs.c once.c
	cc -c -O lmain.c
sub1.o: sub1.c ldefs.c
	cc -c -O sub1.c
sub2.o: sub2.c ldefs.c
	cc -c -O sub2.c
header.o: header.c ldefs.c
	cc -c -O header.c

install: all
	install -s lex $(DESTDIR)/usr/bin
	install -c ncform $(DESTDIR)/usr/lib/lex
	install -c nrform $(DESTDIR)/usr/lib/lex

clean:
	rm -f *.o lex y.tab.c

depend:
