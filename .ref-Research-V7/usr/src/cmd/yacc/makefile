CFLAGS=-O
all: yacc
	:

cmp: all
	cmp yacc /bin/yacc
	rm yacc *.o

cp:	all
	cp yacc /bin/yacc
	rm yacc *.o

yacc: y1.o y2.o y3.o y4.o
	cc -i -s -o yacc y?.o

y1.o y2.o y3.o y4.o: dextern files
