CC=cc
CFLAGS=-O -Dunix=1 -Dvax=1

cpp :	cpp.o cpy.o rodata.o
	$(CC) $(CFLAGS) -o cpp rodata.o cpp.o cpy.o  

cpy.c : cpy.y
	yacc cpy.y
	../as/:yyfix yyexca yyact yypact yypgo yyr1 yyr2 yychk yydef
	mv y.tab.c cpy.c
cpy.o :	cpy.c yylex.c
rodata.o : cpy.c
	$(CC) $(CFLAGS) -S rodata.c
	../as/:rofix rodata.s
	as -o rodata.o rodata.s

clean :
	rm -f *.s *.o cpy.c rodata.c

install :
	install -s cpp $(DESTDIR)/lib

