CFLAGS = -O
all: lex libln.a

cmp:	all
	cmp lex /bin/lex
	ls -l libln.a /lib/libln.a
	rm -f *.o libln.a lex y.tab.c

cp:	all
	cp lex /bin/lex
	cp libln.a /lib
	rm -f *.o libln.a lex y.tab.c

lex: lmain.o y.tab.o sub1.o sub2.o header.o
	cc -i -s lmain.o y.tab.o sub1.o sub2.o header.o -o lex

smallex:
	cc -DSMALL -n -s -O lmain.c y.tab.c sub1.c sub2.c header.c -o smallex

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

libln.a:
	cc -c -O lib/allprint.c lib/main.c lib/reject.c lib/yyless.c
	cc -c -O lib/yywrap.c
	rm -f libln.a
	ar rvc libln.a allprint.o main.o reject.o yyless.o yywrap.o
	rm allprint.o main.o reject.o yyless.o yywrap.o
