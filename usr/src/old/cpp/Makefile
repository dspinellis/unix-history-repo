#	Makefile	1.4	87/05/30
#

YYFIX=	../pcc/pcc.`machine`/:yyfix
CFLAGS=	-O -Dunix=1 -DFLEXNAMES

cpp :	cpp.o cpy.o rodata.o
	$(CC) $(CFLAGS) -o cpp rodata.o cpp.o cpy.o  

cpy.c : cpy.y
	yacc cpy.y
	${YYFIX} yyexca yyact yypact yypgo yyr1 yyr2 yychk yydef
	mv y.tab.c cpy.c
cpy.o :	cpy.c yylex.c
rodata.o : cpy.c
	$(CC) $(CFLAGS) -R -c rodata.c

clean :
	rm -f *.s *.o cpy.c rodata.c

install :
	install -s cpp $(DESTDIR)/lib

depend :

sources : cpp.c cpy.y yylex.c README
cpp.c cpy.y yylex.c README :
	sccs get $@
