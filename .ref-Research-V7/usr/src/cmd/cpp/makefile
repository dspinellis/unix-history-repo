CFLAGS=-O -n -s

all:	cpp

cp:	cpp
	cp cpp /lib
	rm *.o cpp

cmp:	cpp
	cmp cpp /lib/cpp
	rm *.o cpp

cpp: cpp.o cpy.o
	cc $(CFLAGS) cpp.o cpy.o -o cpp
cpp.o: cpp.c
	cc $(CFLAGS) -c -Dunix=1 -Dpdp11=1 cpp.c
cpy.o: cpy.y yylex.c
